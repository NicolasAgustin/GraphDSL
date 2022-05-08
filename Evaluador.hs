
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Evaluador where
import Utils ( flattenLines, format, append, joinLines, replace, groupByColor )
import Control.Applicative (pure, (<*>))
import Control.Monad (liftM, ap, when, unless)
import Control.Monad.Except (ExceptT)
import Ast
import Control.Exception (catch, IOException, evaluate)
import Matriz ( build, empty, set, Matrix(M) )
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
import OrientationMapper (process)

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
    ("OUTPUT", Output []),
    ("MATRIX", Grid (M [])),
    ("EDGES", Cadena ""),
    ("DIST", Entero 0),
    ("COLORS", Colors [])
    ]

colors = ["red","blue","green","yellow","black","white","brown","purple","grey","orange","pink"]

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

-- No se utiliza, funcion para intentar indicar cual seria el tipo esperado
assert :: DataType' -> Either String DataType' -> Eval Bool
assert tipo toAssert = do case toAssert of
                            Left e -> throwE e
                            Right v -> return (tipo == v)

-- Obtener un valor del estado
getValueFromState :: String -> Eval (Either String DataType')
getValueFromState varName = lift $ gets (lookState varName)

-- Actualizar un valor del estado
updateValueFromState :: String -> DataType' -> Eval ()
updateValueFromState varName value = modify (updateState varName value)

{-
    Descripcion:
        Funcion para acumular valores en la variable OUTPUT 
    Args:
        Valor a acumular
    Return: 

-}
writeOutput :: String -> Eval ()
writeOutput str = do output <- getValueFromState "OUTPUT"
                     dato <- typeChecker output (Output [])
                     out <- outputGet dato
                     updateValueFromState "OUTPUT" (Output $ new out)
                        where new out = out ++ [str]


{-
    Descripcion:
        Evaluador de comandos 
    Args:
        Ast
    Return: 
-}
evalComm :: Cmd -> Eval ()
evalComm Pass                       = do return ()
-- Definicion de enteros
evalComm (Let v e)                  = do val <- evalIntExp e
                                         -- Actualizamos el estado
                                         updateValueFromState v (Entero val)
-- Salida hacia archivo de log (solamente si se utiliza el comando log)
evalComm (Log str)                  = do s <- evalStrExp str
                                         -- Obtenemos el valor de la variable bandera LOGGER
                                         name <- getValueFromState "LOGGER"
                                         dato <- typeChecker name (Entero 0)
                                         logger_flag <- enteroGet dato
                                         -- Cambiamos el valor, para que la proxima vez solamente hagamos append
                                         updateValueFromState "LOGGER" (Entero 1)
                                         if logger_flag == 0 then
                                             do lift.lift $ writeFile "logger.lg" ""
                                                lift.lift $ append "logger.lg" (s ++ "\n")
                                         else lift.lift $ append "logger.lg" (s ++ "\n")
-- Definicion de strings
evalComm (LetStr v stre)            = do str <- evalStrExp stre
                                         updateValueFromState v (Cadena str)
-- Secuenciacion de comandos
evalComm (Seq l r)                  = do evalComm l
                                         evalComm r
-- Condicional
evalComm (If b tc fc)               = do bval <- evalBoolExp b
                                         if bval then evalComm tc
                                         else evalComm fc
-- Bucle for
evalComm (For f l c)                = do -- Limite inferior
                                         from <- evalIntExp f
                                         -- Limite superior  
                                         limit <- evalIntExp l
                                         -- Si los limites son distintos, seguir iterando
                                         when (from /= limit) $ do evalComm (Seq c (For (Const (from+1)) l c))
-- Bucle while
evalComm (While cond cmd)           = do condicion <- evalBoolExp cond
                                         when condicion $ do evalComm (Seq cmd (While cond cmd))
-- Insercion de nodos
evalComm (LetNodeCoord id x y)      = do
    -- Id del nodo
    nodeName <- evalStrExp id
    -- filas
    x_value <- evalIntExp x
    -- columnas
    y_value <- evalIntExp y
    matrix <- getValueFromState "MATRIX"
    dato <- typeChecker matrix (Grid empty)
    final_matrix <- gridGet dato
    -- Agregamos el nodo en la matriz
    updateValueFromState "MATRIX" (Grid $ set (x_value, y_value) (Node nodeName x_value y_value) final_matrix)
    updateValueFromState nodeName (Node nodeName x_value y_value)
evalComm (Color colorexp node) = do
    color <- evalStrExp colorexp
    n <- evalNodexp node
    if map toLower color `notElem` colors then throwE "Color no valido"
    else do color_list <- getValueFromState "COLORS"
            dato <- typeChecker color_list (Colors [])
            colors <- colorGet dato 
            updateValueFromState "COLORS" (Colors (colors ++ [(color, n)]))
-- Insercion de aristas
evalComm (Set edge_color tagexp ndexp) = do
    -- Tag para la arista (peso)
    tag <- addOptions edge_color tagexp
    -- Generamos la string que necesita latex
    tw <- evalNodexp ndexp
    edges <- getValueFromState "EDGES"
    dato <- typeChecker edges (Cadena "")
    f_edges <- cadenaGet dato
    -- La arista viene con ! indicando donde poner el tag en caso de que se haya indicado uno
    -- Caso contrario reemplaza ! por ""
    updateValueFromState "EDGES" (Cadena (f_edges ++ replace tw ('!', tag)))
-- Definicion de grafo
evalComm (Graph name distancia msize cmd) = do
    -- Nombre del grafo
    strName <- evalStrExp name
    -- Distancia entre nodos
    dist <- evalIntExp distancia
    -- Tamanio de la matriz 
    size <- evalIntExp msize
    updateValueFromState "DIST" (Entero dist)
    updateValueFromState "NAME" (Cadena strName)
    -- Construimos la matrix y actualizamos el estado
    updateValueFromState "MATRIX" (Grid (build (Node "" 0 0) size))
    -- Header para archivo .tex
    writeOutput $ "\\documentclass{article}\n\\usepackage{tkz-graph}\n\\begin{document}\n \
    \ \\begin{tikzpicture}[node distance={" ++ show dist ++ "mm} ,main/.style = {draw, circle}]\n \
    \ \\SetGraphUnit{" ++ show size ++ "}"
    --------------- EVALUACION DE LOS COMANDOS DEL GRAFICO --------------------
    evalComm cmd
    ---------------------------------------------------------------------------
    
    ---------------------------------------------------------------------------
    matrix <- getValueFromState "MATRIX"
    dato <- typeChecker matrix (Grid empty)
    final_matrix <- gridGet dato
    -- Proceso para establecer posiciones relativas entre nodos
    catchE (writeOutput $ joinLines (process final_matrix) '\n') (\x -> throwE $ "Indices fuera de limite. " ++ x)
    edges <- getValueFromState "EDGES"
    dato <- typeChecker edges (Cadena "")
    f_edges <- cadenaGet dato
    writeOutput f_edges
    -- FALTA AGREGAR LA AGRUPACION POR COLORES
    colores <- getValueFromState "COLORS"
    dato <- typeChecker colores (Colors [])
    f_colores <- colorGet dato 
    writeOutput $ joinLines (genColorString f_colores) '\n'
    --------------------------------------------
    name <- getValueFromState "NAME"
    dato <- typeChecker name (Cadena "")
    name <- cadenaGet dato
    -- Header para el archivo .tex
    writeOutput "\\end{tikzpicture}\n\\end{document}\n"
    lift.lift $ writeFile (strName ++ ".tex") ""
    output <- getValueFromState "OUTPUT"
    dato <- typeChecker output (Output [])
    out_data <- outputGet dato
    -- Escribimos todo lo acumulado en OUTPUT
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

{-
    Descripcion: 
        Funcion para parsear (en el caso que se haya indicado un tag) el texto que sera el tag de la arista.
    Args: 
        Maybe, expresion con el texto para el tag
    Return:
        Texto parseado
-}
addOptions :: Maybe StringExp -> Maybe StringExp -> Eval String
addOptions color tag = do case tag of
                            Nothing -> return ""
                            Just s  -> do tag <- evalStrExp s
                                          case color of
                                            Nothing -> return (format ",label=$%$" [tag])
                                            Just c  -> do
                                                        evaluated_color <- evalStrExp c
                                                        colorChecker evaluated_color
                                                        return (format ",label=$%$,color=%" [tag, evaluated_color])

colorChecker :: String -> Eval ()
colorChecker color = do when (color `notElem` colors) $ throwE "Color no definido"

cadenaGet :: DataType' -> Eval String
cadenaGet (Cadena s) = return s

enteroGet :: DataType' -> Eval Integer
enteroGet (Entero i) = return i

outputGet :: DataType' -> Eval [String]
outputGet (Output l) = return l

nodeGet :: DataType' -> Eval (String, Integer, Integer)
nodeGet (Node i x y) = return (i, x, y)

colorGet :: DataType' -> Eval [(String, String)]
colorGet (Colors f) = return f 

gridGet :: DataType' -> Eval Mapper
gridGet (Grid m) = return m

-- Chequeador de tipos
typeChecker :: Either String DataType' -> DataType' -> Eval DataType'
typeChecker (Left s) _ = throwE s
typeChecker (Right d) s = do
    if toConstr d == toConstr s then return d
    else throwE (
        format "No coinciden los tipos. Se recibio % pero se esperaba %." [show (toConstr d), show (toConstr s)]
        )
{-
    Descripcion:
        Funcion para generar la string necesaria para el archivo .tex
    Args:
        (String, String), Id nodo1 y nodo2
        String, Operador (<-,->,-)
    Return:
        String para latex
-}
genNodeString :: (String, String) -> String -> Eval String
genNodeString (n1, []) op = throwE (format "Falta el operando derecho para %." [op])
genNodeString ([], n2) op = throwE (format "Falta el operando izquierdo para %." [op])
genNodeString (n1, n2) op = do
    if n1 == n2 then do
        m_dist <- getValueFromState "DIST"
        dato <- typeChecker m_dist (Entero 0)
        dist <- enteroGet dato
        return $ format "\\Loop[style={%},dist=%cm!](%)(%)\n" [op, show dist, n1, n1]
    else return (format "\\Edge[style={%}!](%)(%)\n" [op, n1, n2])

genColorString :: [(String, String)] -> [String]
genColorString l = genColorString' (groupByColor l)

genColorString' :: [[(String, String)]] -> [String] 
genColorString' []       = []
genColorString' (f:rest) = ("\\AddVertexColor{" ++ color ++ "}{" ++ nodos_str ++ "}") : genColorString' rest
                            where 
                                -- Obtenemos el primer color que aparezca y construimos la lista con los id de nodos
                                (color, nodos) = (getNodeColor f, map snd f)
                                -- Formateamos la lista para obtener algo con la forma: n1,n2,n2
                                -- Tambien obtenemos todos excepto el ultimo elemento para eliminar la "," de mas
                                nodos_str = init $ foldr (\x y -> x ++ "," ++ y) "" nodos 

getNodeColor :: [(String, String)] -> String
getNodeColor (f:rest) = fst f

-- Evaluacion de expresiones con nodos
evalNodexp :: Nodexp -> Eval String
evalNodexp (LeftTo nl nr) = do
        node1 <- evalNodexp nl
        node2 <- evalNodexp nr
        genNodeString (node1, node2) "->"
evalNodexp (RightTo nl nr) = do
        node1 <- evalNodexp nl
        node2 <- evalNodexp nr
        genNodeString (node1, node2) "<-"
evalNodexp (LeftRight nl nr) = do
        node1 <- evalNodexp nl
        node2 <- evalNodexp nr
        genNodeString (node1, node2) "-"
evalNodexp (NodeVar var) = do
        nodeVar <- getValueFromState var
        str <- typeChecker nodeVar (Node "" 0 0)
        (i, d, t) <- nodeGet str
        return i
evalNodexp (ConstNode str) = do
    rresult <- evalStrExp str
    nodeContent <- getValueFromState rresult
    node <- typeChecker nodeContent (Node "" 0 0)
    (i, d, t) <- nodeGet node
    return i

-- Evaluacion de expresiones string
evalStrExp :: StringExp -> Eval String
evalStrExp (Str s)          = return s
evalStrExp (Concat l r)     = do sl <- evalStrExp l
                                 sr <- evalStrExp r
                                 return (sl ++ sr)
evalStrExp (VariableStr sv) = do r <- getValueFromState sv
                                 dato <- typeChecker r (Cadena "")
                                 cadenaGet dato
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
evalIntExp (Mod l r)     = do e1 <- evalIntExp l
                              e2 <- evalIntExp r
                              return (e1 `mod` e2)
evalIntExp (Uminus l)    = do e1 <- evalIntExp l
                              return (negate e1)
evalIntExp (Len str)     = do s <- evalStrExp str
                              return (toInteger $ length s)
evalIntExp (IntCast str) = do s <- evalStrExp str
                              case readMaybe s of
                                    Just num -> return num
                                    Nothing  -> throwE ("Error al castear la string \"" ++ s ++ "\"")
evalIntExp (Variable v)  = do r <- getValueFromState v
                              dato <- typeChecker r (Entero 0)
                              enteroGet dato

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