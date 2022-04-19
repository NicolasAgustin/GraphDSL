
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Evaluador where
import Utils ( flattenLines, format, append )
import Control.Applicative (pure, (<*>))
import Control.Monad (liftM, ap, when)
import Control.Monad.Except (ExceptT)
import Ast
import Data.String (lines)
import Data.Data ( Data(toConstr) )
import Data.Dynamic ()
import Parser ()
import Control.Monad.Trans (MonadTrans)
import Control.Monad.Trans.Class (lift)
import Control.Monad.State (StateT)
import Control.Monad.State.Lazy
    ( modify, StateT(runStateT), put, get )
import Control.Monad.Trans.Except ( throwE, runExceptT, catchE )
import Control.Monad.Trans.State.Lazy (gets)
import Text.Read (readMaybe)
import GHC.IO (catchAny)
import System.IO ()
import Data.Char (toLower, isSpace)
import System.Process ( callCommand )

{-
    TODO:
    - CAMBIAR TIKZ POR TKZ-GRAPH y cambiar direcciones por indices de matriz (below left por x=0 y=0)
    - Ver si se puede cambiar la distancia entre nodos de forma dinamica (podria ser una primitiva)
        Con xshift y yshift podemos manejar el espaciado
            ejemplo = \node[main] (node1) [below of=node0, yshift=5mm] {$node1$}; 
            Lo que se podria hacer es setear una variable y cada vez que se crea un nodo setear la distancia buscando esa variable
            Definir primitiva para controlar el valor de esta variable
    - Definir primitiva para cambiar el tamanio de los nodos
        Con scale se puede cambiar el tamanio de un nodo
            ejemplo = \node[main] (node1) [below of=node0, yshift=5mm, scale=0.3] {$node1$};
    - Ver como se puede cambiar el color de una arista, nodo o tag (primitiva)
        Para cambiar el color de un nodo se puede usar fill, pasandole los 3 valores rgb
            ejemplo = \node[main] (node1) [below of=node0, yshift=5mm, scale=0.3, fill={rgb:red,4;green,2;yellow,1}] {$node1$};
    - Ver como agregar tag a una arista (agregar a la primitiva de edge)
-}

-- Tipo de dato para agrupar las monadas (transformadores)
type Eval a = ExceptT String (StateT Env IO) a

-- Tipo de dato para el estado 
type Env = [(String, DataType')]

-- Inicializacion del estado
initState :: Env
initState = [
    ("COUNTER", Entero 0),
    ("ERR", Cadena ""),
    ("LOGGER", Entero 0),
    ("OUTPUT", Output [])
    ]

eval :: Cmd -> Eval ()
eval p = do evalCommInit p

{- Funciones auxiliares para manejar el estado de StateT -}

-- Actualizar el estado
updateState :: String -> DataType' -> Env -> Env
updateState var tipo []          = [(var, tipo)]
updateState var tipo ((x,x2):xs) = if var == x
                                    then (var, tipo):xs
                                    else (x,x2): updateState var tipo xs

-- Buscar en el estado
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

assert :: DataType' -> Either String DataType' -> Eval Bool
assert tipo toAssert = do case toAssert of
                            Left e -> throwE e
                            Right v -> return (tipo == v)

getValueFromState :: String -> Eval (Either String DataType')
getValueFromState varName = lift $ gets (lookState varName)

updateValueFromState :: String -> DataType' -> Eval ()
updateValueFromState varName value = modify (updateState varName value)

writeOutput :: String -> Eval ()
writeOutput str = do output <- getValueFromState "OUTPUT"
                     (b, dato) <- typeChecker output (Output [])
                     out <- outputGet dato
                     updateValueFromState "OUTPUT" (Output $ new out)
                        where new out = out ++ [str]

-- Evaluador de comandos
evalComm :: Cmd -> Eval ()
evalComm Pass                       = do return ()
evalComm (Let v e)                  = do val <- evalIntExp e                  -- Definicion de enteros
                                         updateValueFromState v (Entero val)  -- Actualizamos el estado
evalComm (Log str)                  = do s <- evalStrExp str                  -- Salida por pantalla
                                         name <- getValueFromState "LOGGER"
                                         (b, dato) <- typeChecker name (Entero 0)
                                         logger_flag <- enteroGet dato
                                         updateValueFromState "LOGGER" (Entero 1)
                                         if logger_flag == 0 then
                                             do lift.lift $ writeFile "logger.lg" ""
                                                lift.lift $ append "logger.lg" (s ++ "\n")
                                         else lift.lift $ append "logger.lg" (s ++ "\n")
evalComm (LetStr v stre)            = do str <- evalStrExp stre               -- Definicion de strings
                                         updateValueFromState v (Cadena str)
evalComm (Seq l r)                  = do evalComm l
                                         evalComm r
evalComm (If b tc fc)               = do bval <- evalBoolExp b
                                         if bval then evalComm tc
                                         else evalComm fc
evalComm (For f l c)                = do from <- evalIntExp f     -- Limite inferior
                                         limit <- evalIntExp l    -- Limite superior
                                         -- Si los limites son distintos, seguir iterando
                                         when (from /= limit) $ do evalComm (Seq c (For (Const (from+1)) l c))
evalComm (While cond cmd)           = do condicion <- evalBoolExp cond
                                         when condicion $ do evalComm (Seq cmd (While cond cmd))
evalComm (LetNodeCoord id x y)      = do
    nodeName <- evalStrExp id
    x_value <- evalIntExp x
    y_value <- evalIntExp y
    updateValueFromState nodeName (Node nodeName x_value y_value)
    writeOutput (format "\\Vertex[x=%, y=%]{%}\n" [show x_value, show y_value, nodeName])
-- evalComm (LetNode id dir tag)       = do
--     nodeName <- evalStrExp id   -- Identificador del nodo
--     ttag <- evalStrExp tag      -- Tag que se va a visualizar en el grafico
--     sid <- makeStringDirections dir
--     updateValueFromState nodeName (Node nodeName sid ttag)
--     writeOutput $ "\\node[main] (" ++ nodeName ++ ") [" ++ sid ++ "] " ++ "{$" ++ ttag ++ "$};\n"
evalComm (Set ndexp)                = do
    tw <- evalNodexp ndexp
    writeOutput tw
evalComm (Graph name distancia cmd) = do
    strName <- evalStrExp name     -- Nombre del grafico
    dist <- evalIntExp distancia   -- Distancia entre nodos
    updateValueFromState "NAME" (Cadena strName)
    writeOutput $ "\\documentclass{article}\n\\usepackage{tkz-graph}\n\\begin{document}\n \
    \ \\begin{tikzpicture}[node distance={" ++ show dist ++ "mm} ,main/.style = {draw, circle}]\n"
    evalComm cmd
    name <- getValueFromState "NAME"
    (b, dato) <- typeChecker name (Cadena "")
    name <- cadenaGet dato
    writeOutput "\\end{tikzpicture}\n\\end{document}\n"
    lift.lift $ writeFile (strName ++ ".tex") ""
    output <- getValueFromState "OUTPUT"
    (b, dato) <- typeChecker output (Output [])
    out_data <- outputGet dato
    lift.lift $ append (strName ++ ".tex") (flattenLines out_data)
    {- Llamamos al compilador de latex con el nombre del archivo .tex creado y redireccionando la salida
    a nul -}
    lift.lift $ callCommand $ "pdflatex " ++ (name ++ ".tex > nul 2>&1")
    -- Eliminamos los archivos .aux 
    lift.lift $ callCommand "del *.aux > nul 2>&1"
    -- Eliminamos los archivos .dvi
    lift.lift $ callCommand "del *.dvi > nul 2>&1"
    -- Eliminamos los archivos .log
    lift.lift $ callCommand "del *.log > nul 2>&1"

cadenaGet :: DataType' -> Eval String
cadenaGet (Cadena s) = return s

enteroGet :: DataType' -> Eval Integer
enteroGet (Entero i) = return i

outputGet :: DataType' -> Eval [String]
outputGet (Output l) = return l

nodeGet :: DataType' -> Eval (String, Integer, Integer)
nodeGet (Node i x y) = return (i, x, y)

typeChecker :: Either String DataType' -> DataType' -> Eval (Bool, DataType')
typeChecker (Left s) _ = throwE s
typeChecker (Right d) s = if toConstr d == toConstr s then return (True, d)
                          else throwE $ "No coinciden los tipos. Se recibio " ++ show (toConstr d) ++ " pero se esperaba " ++ show (toConstr s) ++ "."

-- Funcion auxiliar para obtener la string de direcciones
{- 
    Ejemplo: 
        ([Above, Right], "n1") -> "above right of=n1"
-}
makeStringDirections :: Maybe ([Position], Nodexp) -> Eval String
makeStringDirections dir = do case dir of
                                Nothing -> return ""
                                Just (p, sid) -> do
                                    s <- evalNodexp sid
                                    nodo <- getValueFromState s
                                    (b, nodo_d) <- typeChecker nodo (Node "" 0 0)
                                    -- Dropeamos todos los espacios de la izquierda
                                    return (dropWhile isSpace posiciones ++ " of=" ++ s)
                                        where
                                            {- Esta funcion se encarga de procesar la lista de [Position]
                                                primero mapea la funcion (" "++).show en la lista, obteniendo asi la representacion
                                                de string de cada Position separada por un espacio:
                                                [Above, Right] -> [" above"," right"]
                                                Luego va plegando la lista hacia la derecha concatenando todos los elementos
                                                [" above"," right"] -> " above right"
                                                Por ultimo se eliminan los espacios de la izquierda
                                            -}
                                            posiciones = foldr (++) [] (map ((" "++).show) p)

-- Evaluacion de expresiones con nodos
evalNodexp :: Nodexp -> Eval String
evalNodexp (LeftTo nl nr) = do node1 <- evalNodexp nl
                               node2 <- evalNodexp nr
                               return (makeString node1 node2)
                                    where
                                        makeString n1 n2 = "\\Edge[style={->}](" ++ n1 ++ ")(" ++ n2 ++ ")\n"
evalNodexp (RightTo nl nr) = do node1 <- evalNodexp nl
                                node2 <- evalNodexp nr
                                return (makeString node1 node2)
                                    where
                                        makeString n1 n2 = "\\Edge[style{<-}](" ++ n2 ++ ")(" ++ n1 ++ ")\n"
evalNodexp (LeftRight nl nr) = do node1 <- evalNodexp nl
                                  node2 <- evalNodexp nr
                                  return (makeString node1 node2)
                                    where
                                        makeString n1 n2 = "\\Edge[style={-}](" ++ n2 ++ ")(" ++ n1 ++ ")\n"
evalNodexp (NodeVar var) = do nodeVar <- getValueFromState var
                              (_, str) <- typeChecker nodeVar (Node "" 0 0)
                              (i, d, t) <- nodeGet str
                              return i
evalNodexp (ConstNode str) = do rresult <- evalStrExp str
                                strContent <- getValueFromState rresult
                                (_, str) <- typeChecker strContent (Cadena "")
                                str_content <- cadenaGet str
                                nodeVar <- getValueFromState str_content
                                (b, ndata) <- typeChecker nodeVar (Node "" 0 0)
                                (i, d, t) <- nodeGet ndata
                                return i

-- Evaluacion de expresiones string
evalStrExp :: StringExp -> Eval String
evalStrExp Error = throwE "Error evaluando expresion string"
evalStrExp (Str s)          = return s
evalStrExp (Concat l r)     = do sl <- evalStrExp l
                                 sr <- evalStrExp r
                                 return (sl ++ sr)
evalStrExp (VariableStr sv) = do r <- getValueFromState sv
                                 (b, dato) <- typeChecker r (Cadena "")
                                 if b then do cadenaGet dato
                                 else do updateValueFromState "ERR" (Cadena "No coinciden los tipos")
                                         throwE $ "No coincide el tipo de \"" ++ sv ++ "\" esperado string."
evalStrExp (StrCast n)      = do res <- evalIntExp n
                                 return (show res)

-- Evaluacion de expresiones enteras
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
                             if e2 == 0 then
                                 do updateValueFromState "ERR" (Cadena "No se puede dividir por cero")
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
evalIntExp (Variable v)  = do r <- getValueFromState v
                              (b, dato) <- typeChecker r (Entero 0)
                              if b then do enteroGet dato
                              else do updateValueFromState "ERR" (Cadena "No coinciden los tipos")
                                      throwE "No coinciden los tipos"

-- Evaluacion de expresiones booleanas
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

-- Funcion para debuggear
mprint :: String -> Eval ()
mprint s = lift.lift $ putStrLn s