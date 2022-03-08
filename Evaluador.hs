
module Evaluador where
import Control.Applicative (pure, (<*>))
import Control.Monad (liftM, ap, when)
import Control.Monad.Except (ExceptT)
import Ast
import Parser
import Control.Monad.Trans (MonadTrans)
import Control.Monad.Trans.Class (lift)
import Control.Monad.State (StateT)
import Control.Monad.State.Lazy
    ( modify, StateT(runStateT), put, get )
import Data.Data (Data)
import Control.Monad.Error ( throwError, catchError )
import Control.Monad.Trans.Error (runErrorT)
import Control.Monad.Trans.Except ( throwE, runExceptT, catchE )
import Control.Monad.Trans.State.Lazy (gets)
import Text.Read (readMaybe)
import GHC.IO (catchAny)
import System.IO
import Data.Char (toLower)

type Eval a = ExceptT String (StateT Env IO) a

type Env = [(String, DataType')]

data EvalError = DivideByZero | VariableNotDefined String deriving Show

type VariableF = (String, Integer)

data DataType' = Entero Integer | Cadena String deriving (Show)

{- TODO:
    - Revisar si el control de errores funciona dentro del if y del for -}

instance Eq DataType' where
    (==) (Entero _) (Entero _) = True
    (==) (Entero _) (Cadena _) = False
    (==) (Cadena _) (Entero _) = False
    (==) (Cadena _) (Cadena _) = True

initState :: Env
initState = [("COUNTER", Entero 0), ("ERR", Cadena "")]

eval :: Cmd -> Eval ()
eval p = do evalCommInit p

-- Funciones auxiliares para manejar el estado de StateT
updateState :: String -> DataType' -> Env -> Env
updateState var tipo []          = [(var, tipo)]
updateState var tipo ((x,x2):xs) = if var == x
                                    then (var, tipo):xs
                                    else (x,x2): updateState var tipo xs
                                    -- aca puede ser que no coincidan los tipos, hay que poner un either

lookState :: String -> Env -> Either String DataType'
lookState var []            = Left ("Variable no definida: " ++ var)
lookState var ((s,s1):st) = if s == var
                                then Right s1
                                else lookState var st

---------------------------------------------------------------

-- Inicializa el estado y llama al evaluador de comandos
evalCommInit :: Cmd -> Eval ()
evalCommInit p = do put initState
                    evalComm p

evalComm :: Cmd -> Eval ()
evalComm Pass            = do return ()
evalComm (Let v e)       = do val <- evalIntExp e
                              modify (updateState v (Entero val))
evalComm (Print str)     = do s <- evalStrExp str
                              lift.lift $ putStrLn s
                              lift.lift $ hFlush stdout
evalComm (LetStr v stre) = do str <- evalStrExp stre
                              modify (updateState v (Cadena str))
evalComm (Seq l r)       = do evalComm l
                              evalComm r
evalComm (If b tc fc)    = do bval <- evalBoolExp b
                              if bval then evalComm tc
                              else evalComm fc
evalComm (For f l c)       = do from <- evalIntExp f
                                limit <- evalIntExp l
                                when (from /= limit) $ do evalComm (Seq c (For (Const (from+1)) l c))
evalComm (While cond cmd)  = do condicion <- evalBoolExp cond
                                when condicion $ do evalComm (Seq cmd (While cond cmd))
    -- (var, value) <- evalForDef $ (\(Forc d _ _) -> d) b
    --                             counter <- lift $ gets (lookState "COUNTER")
    --                           case counter of
    --                             Left e     -> throwE e
    --                             Right fine -> case fine of
    --                                             Entero valor -> do
    --                                                 when (valor == 0) $ modify (updateState var (Entero value))
    --                                                 condicion <- evalForCondition $ (\(Forc _ c _) -> c) b
    --                                                 (var', value') <- evalForInc $ (\(Forc _ _ i) -> i) b
    --                                                 when condicion $ do
    --                                                     modify (updateState var' (Entero value'))
    --                                                     modify (updateState "COUNTER" (Entero (valor+1)))
    --                                                     evalComm (Seq c (For b c))
    --                                             Cadena str -> throwE "Error inesperado"
evalComm (WriteFile path tw append) = do ruta <- evalStrExp path
                                         twrite <- evalStrExp tw
                                         b <- evalBoolExp append
                                         if b then lift.lift $ appendFile ruta twrite
                                         else lift.lift $ writeFile ruta twrite
                                         return ()
evalComm (LetNode id dir tag) = do  ttag <- evalStrExp tag
                                    sid <- makeString dir
                                    lift.lift $ appendFile "./latex/grafos.tex" (write ttag sid) 
                                            where 
                                                write t sid = "\\node[main] (" ++ id ++ ") [" ++ sid ++ "] " ++ "{$" ++ t ++ "$};"
evalComm (Set ndexp) = do tw <- evalNodexp ndexp
                          lift.lift $ appendFile "./latex/grafos.tex" tw
evalComm Init = do lift.lift $ appendFile "./latex/grafos.tex" tw
                    where 
                        tw = "\\documentclass{article}\\usepackage{tikz}\\begin{document}\\begin{tikzpicture}[node distance={15mm} ,main/.style = {draw, circle}]"
evalComm End = do lift.lift $ appendFile "./latex/grafos.tex" tw
                    where 
                        tw = "\\end{tikzpicture}\\end{document}"

makeString :: Maybe (Position, StringExp) -> Eval String 
makeString dir = do case dir of 
                        Nothing -> return ""
                        Just (p, sid) -> do s <- evalStrExp sid 
                                            return (map toLower (show p) ++ " of=" ++ s)

evalNodexp :: Nodexp -> Eval String
evalNodexp (LeftTo nl nr) = do node1 <- evalNodexp nl 
                               node2 <- evalNodexp nr 
                               return (makeString node1 node2) 
                                where 
                                    makeString n1 n2 = "\\draw[->] (" ++ n1 ++ ") -- (" ++ n2 ++ ");"
evalNodexp (RightTo nl nr) = do return ""
evalNodexp (NodeVar var) = do return var
evalNodexp (ConstNode str) = do return ""
evalNodexp (Node str str2 str3 i) = do return ""

 
{-
LeftTo Nodexp Nodexp 
              | RightTo Nodexp Nodexp
              | NodeVar Var
              | ConstNode StringExp
              | Node StringExp StringExp StringExp Iexp
-}


evalForCond :: Forcond -> Eval (VariableF, Bool, VariableF)
evalForCond (Forc d c i) = do t <- evalForDef d
                              b <- evalForCondition c
                              t2 <- evalForInc i
                              return (t, b, t2)

-- Aca devolvemos el nombre de la variable para poder pasarsela al evaluador de incremento
evalForDef :: Definicion -> Eval VariableF
evalForDef (Def2 pdef var exp) = do d <- lift $ gets (lookState var)
                                    valor <- evalIntExp exp
                                    case d of
                                        Left e     -> if pdef
                                                        then do modify (updateState var (Entero valor))
                                                                return (var, valor)
                                                        else throwE e
                                        Right fine -> return (var, valor)

evalForCondition :: Condicion -> Eval Bool
evalForCondition (Cond exp) = do evalBoolExp exp

evalForInc :: Definicion -> Eval VariableF
evalForInc (Def2 pdef var exp) = do valor <- evalIntExp exp
                                    return (var, valor)

evalStrExp :: StringExp -> Eval String
evalStrExp (Str s)          = return s
evalStrExp (Concat l r)     = do sl <- evalStrExp l
                                 sr <- evalStrExp r
                                 return (sl ++ sr)
evalStrExp (VariableStr sv) = do r <- lift $ gets (lookState sv)
                                 case r of
                                     Left e     -> throwE e
                                     Right fine -> case fine of
                                                    Cadena str -> return str
                                                    Entero i   -> do
                                                        modify (updateState "ERR" (Cadena "No coinciden los tipos"))
                                                        throwE $ "No coincide el tipo de \"" ++ sv ++ ": " ++ show (Entero i) ++ "\" con string."
evalStrExp (StrCast n)      = do res <- evalIntExp n
                                 return (show res)
evalStrExp (Input str)      = do prompt <- evalStrExp str
                                 lift.lift $ putStr prompt
                                 lift.lift $ hFlush stdout
                                 lift.lift $ getLine
evalStrExp (ReadFile path)  = do ruta <- evalStrExp path
                                 lift.lift $ readFile ruta

evalIntExp :: Iexp -> Eval Integer
evalIntExp (Const n)    = return n
evalIntExp (Plus l r)   = do e1 <- evalIntExp l
                             e2 <- evalIntExp r
                             return (e1 + e2)
evalIntExp (Minus l r)  = do e1 <- evalIntExp l
                             e2 <- evalIntExp r
                             return (e1 - e2)
evalIntExp (Div l r)    = do e1 <- evalIntExp l
                             e2 <- evalIntExp r
                             if e2 == 0 then do modify (updateState "ERR" (Cadena "No se puede dividir por cero"))
                                                throwE "No se puede dividir por cero"
                             else return (e1 `div` e2)
evalIntExp (Times l r)   = do e1 <- evalIntExp l
                              e2 <- evalIntExp r
                              return (e1 * e2)
evalIntExp (Uminus l)    = do e1 <- evalIntExp l
                              return (negate e1)
evalIntExp (Len str)     = do s <- evalStrExp str
                              return (toInteger $ length s)
evalIntExp (IntCast str) = do s <- evalStrExp str
                              case readMaybe s of
                                    Just num -> return num
                                    Nothing  -> throwE ("Error al castear la string \"" ++ s ++ "\"")
evalIntExp (Variable v)  = do r <- lift $ gets (lookState v)
                              case r of
                                  Left e     -> throwE e
                                  Right fine -> case fine of
                                                    Entero e  -> return e
                                                    Cadena str -> do
                                                        modify (updateState "ERR" (Cadena "No coinciden los tipos"))
                                                        throwE "No coinciden los tipos"

evalBoolExp :: Bexp -> Eval Bool
evalBoolExp Btrue           = return True
evalBoolExp Bfalse          = return False
evalBoolExp (And l r)       = do e1 <- evalBoolExp l
                                 e2 <- evalBoolExp r
                                 return (e1 && e2)
evalBoolExp (Or l r)        = do e1 <- evalBoolExp l
                                 e2 <- evalBoolExp r
                                 return (e1 || e2)
evalBoolExp (Not l)         = do e1 <- evalBoolExp l
                                 return (not e1)
evalBoolExp (Greater l r)   = do e1 <- evalIntExp l
                                 e2 <- evalIntExp r
                                 return (e1 > e2)
evalBoolExp (Less l r)      = do e1 <- evalIntExp l
                                 e2 <- evalIntExp r
                                 return (e1 < e2)
evalBoolExp (GreaterEq l r) = do e1 <- evalIntExp l
                                 e2 <- evalIntExp r
                                 return (e1 >= e2)
evalBoolExp (LessEq l r)    = do e1 <- evalIntExp l
                                 e2 <- evalIntExp r
                                 return (e1 <= e2)
evalBoolExp (Eq l r)        = do e1 <- evalIntExp l
                                 e2 <- evalIntExp r
                                 return (e1 == e2)
evalBoolExp (EqStr l r)        = do e1 <- evalStrExp l
                                    e2 <- evalStrExp r
                                    return (e1 == e2)
evalBoolExp (NotEqStr l r)        = do e1 <- evalStrExp l
                                       e2 <- evalStrExp r
                                       return (e1 /= e2)
evalBoolExp (NotEq l r)     = do e1 <- evalIntExp l
                                 e2 <- evalIntExp r
                                 return (e1 /= e2)