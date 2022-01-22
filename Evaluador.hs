{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
module Evaluador where
import Control.Applicative (pure, (<*>))
import Control.Monad (liftM, ap, when)
import Control.Monad.Except (ExceptT)
import Ast
import Parser
import Control.Error (handleExceptT)

type Env = [(String, DataType')]

data EvalError = DivideByZero | VariableNotDefined String deriving Show

type VariableF = (String, Integer)

data DataType' = Entero Integer | Cadena String deriving (Show)

{- TODO:
    - Ver como evaluar los diferentes tipos, los datatype se pueden manejar con case of
    - Ver como implementar los errores parametrizados -> podria ser seteando una variable de sistema como COUNTER pero para llevar los errores
       entonces cuando hay que tirar un error vamos y recuperamos el error que tiro de esa variable
       implicaria hacer un update de la variable ERROR y luego hacer un lookfor para el throw (dentro de la monada) 
    - Revisar si el control de errores funciona dentro del if y del for -}

instance Eq DataType' where
    (==) (Entero _) (Entero _) = True
    (==) (Entero _) (Cadena _) = False
    (==) (Cadena _) (Entero _) = False
    (==) (Cadena _) (Cadena _) = True

initState :: Env
initState = [("COUNTER", Entero 0), ("ERR", Cadena "")]

newtype Id a = Id a
runId :: Id a -> a
runId (Id a) = a

-- En versiones nuevas del ghc para definir una monada tambien necesitas definirlo como functor y como applicative

instance Functor Set where
    fmap = liftM

instance Applicative Set where
    pure  = return
    (<*>) = ap

newtype Set a = Set { runSet :: Env -> Either String (a, Env, Integer)}

instance Monad Set where
    return x            = Set (\s -> Right (x, s, 0))
    m >>= f = Set (\s -> case runSet m s of
                            Left e           -> Left e
                            Right (a, s', i) -> case runSet (f a) s' of
                                                    Left e' -> Left e'
                                                    Right (b, s'', i') -> Right (b, s'', i' + i))
{- m es un Set a, por lo que con (runSet m) devolvemos una funcion que toma como argumento el estado osea s -}


class Monad m => MonadState m where
    lookfor :: String -> m DataType'
    update :: String -> DataType' -> m ()

lookf :: String -> Env -> Either String DataType'
lookf var [] = Left "Variable no definida"
lookf var ((var', val):ss) | var == var' = Right val
                           | otherwise = lookf var ss

update' :: String -> DataType' -> Env -> Either String Env
update' var val [] = Right [(var, val)]
update' var val ((var', val'):ss) | var == var' = if val' == val then Right ((var, val):ss) else Left ""
                                  | otherwise   = case update' var val ss of
                                                    Left e  -> Left e
                                                    Right r -> Right ((var', val'):r)
                                    --   Right $ (var', val'):update' var val ss

instance MonadState Set where
    lookfor var = Set (\s -> do case lookf var s of
                                    Left e  -> Left e
                                    Right i -> Right (i, s, 0))

    update var val = Set (\s -> let r = update' var val s
                                   in case r of
                                       Left err  -> Left err
                                       Right res -> Right ((), res, 0))

class Monad m => MonadError m where
    throw :: m a

lookupError :: Env -> DataType'
lookupError ((i, v):r) = if i == "ERR" then v else lookupError r

instance MonadError Set where
    throw = Set (\s -> let err = lookupError s
                        in case err of
                            Cadena e -> Left e)

class Monad m => MonadTick m where
    tick :: m ()
    getTick :: m () -> m Integer

instance MonadTick Set where
    tick = Set (\s -> Right ((), s, 1))

eval :: Cmd -> Either String (Env, Integer)
eval p = case runSet (evalComm p) initState of
            Right (v, s, t) -> Right (s, t)
            Left e          -> Left e

evalComm :: (MonadState m, MonadError m, MonadTick m) => Cmd -> m ()
evalComm Pass            = return ()
evalComm (Let v e)       = do val <- evalIntExp e
                              update v (Entero val)
evalComm (LetStr v stre) = do str <- evalStrExp stre
                              update v (Cadena str)
evalComm (Seq l r)       = do evalComm l
                              evalComm r
evalComm (If b tc fc)    = do bval <- evalBoolExp b
                              if bval then evalComm tc
                              else evalComm fc
evalComm (For b c)       = do (var, value) <- evalForDef $ (\(Forc d _ _) -> d) b
                              counter <- lookfor "COUNTER"
                              case counter of
                                  Entero valor -> do when (valor == 0) $ update var (Entero value)
                                                     condicion <- evalForCondition $ (\(Forc _ c _) -> c) b
                                                     (var', value') <- evalForInc $ (\(Forc _ _ i) -> i) b
                                                     when condicion $ do update var' (Entero value')
                                                                         update "COUNTER" (Entero (valor + 1))
                                                                         evalComm (Seq c (For b c))
                                  Cadena str -> throw

evalForCond :: (MonadState m, MonadError m, MonadTick m) => Forcond -> m (VariableF, Bool, VariableF)
evalForCond (Forc d c i) = do t <- evalForDef d
                              b <- evalForCondition c
                              t2 <- evalForInc i
                              return (t, b, t2)
--                               {- Tira error porque al momento de evaluar la expresion no encuentra la variable
--                               hay que ver como definir la variable en el momento de evalForDef, para que de esta
--                               forma la variable ya quede definida y luego no tire error 

--                               - en evalComm llamar a evalForDef por separado, hacer un update con esa variable para
--                               definirla y luego seguir con el resto

--                               -}


-- -- Aca devolvemos el nombre de la variable para poder pasarsela al evaluador de incremento
evalForDef :: (MonadState m, MonadError m, MonadTick m) => Definicion -> m VariableF
evalForDef (Def2 var exp) = do valor <- evalIntExp exp
                               return (var, valor)

evalForCondition :: (MonadState m, MonadError m, MonadTick m) => Condicion -> m Bool
evalForCondition (Cond exp) = do evalBoolExp exp

evalForInc :: (MonadState m, MonadError m, MonadTick m) => Definicion -> m VariableF
evalForInc (Def2 var exp) = do valor <- evalIntExp exp
                               return (var, valor)

evalStrExp :: (MonadState m, MonadError m, MonadTick m) => StringExp -> m String
evalStrExp (Str s)          = return s
evalStrExp (Concat l r)     = do sl <- evalStrExp l
                                 sr <- evalStrExp r
                                 return (sl ++ sr)
evalStrExp (VariableStr sv) = do r <- lookfor sv
                                 case r of
                                     Cadena str -> return str
                                     Entero i   -> do update "ERR" (Cadena "No coinciden los tipos")
                                                      throw -- cuidado porque se le puede pasar un nombre de variable entera, en ese caso deberia tirar error
evalIntExp :: (MonadState m, MonadError m, MonadTick m) => Iexp -> m Integer
evalIntExp (Const n)    = return n
evalIntExp (Plus l r)   = do e1 <- evalIntExp l
                             e2 <- evalIntExp r
                             return (e1 + e2)
evalIntExp (Minus l r)  = do e1 <- evalIntExp l
                             e2 <- evalIntExp r
                             return (e1 - e2)
evalIntExp (Div l r)    = do e1 <- evalIntExp l
                             e2 <- evalIntExp r
                             if e2 == 0 then do update "ERR" (Cadena "No se puede dividir por cero.")
                                                throw
                             else return (e1 `div` e2)
evalIntExp (Times l r)  = do e1 <- evalIntExp l
                             e2 <- evalIntExp r
                             return (e1 * e2)
evalIntExp (Uminus l)   = do e1 <- evalIntExp l
                             return (negate e1)
evalIntExp (Len str)    = do s <- evalStrExp str
                             return (toInteger $ length s)

evalIntExp (Variable v) = do r <- lookfor v
                             case r of
                                 Entero e  -> return e
                                 Cadena str -> do update "ERR" (Cadena "No coinciden los tipos")
                                                  throw

evalBoolExp :: (MonadState m, MonadError m, MonadTick m) => Bexp -> m Bool
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
evalBoolExp (NotEq l r)     = do e1 <- evalIntExp l
                                 e2 <- evalIntExp r
                                 return (e1 /= e2)