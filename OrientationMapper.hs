{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# HLINT ignore "Eta reduce" #-}
module OrientationMapper where
import Matriz ( Matrix , Matrix(M), set, get, empty, build, mmap, search, calculateLimit, dimensions, near, getMatrix)
import Ast (DataType' (Node), Mapper)

import Utils (format)
import GHC.TopHandler (runIO)

insert :: DataType' -> Mapper -> Mapper
insert n@(Node id r c) m = set (r, c) n m
{-
    Funcion para insertar un elemento en Matrix
-}

process :: Mapper -> [String]
process m = iterRows m m 0
{-
    Proceso principal del mapper,
    generar la lista de strings con los nodos que hay en la matriz,
    posicionando los nodos de forma relativa y especificada por puntos
    cardinales
-}


iterRows :: Mapper -> Mapper -> Int -> [String]
iterRows m (M []) _             = []
iterRows m (M (l:rest)) counter = iterCols m l counter 0 ++ iterRows m (M rest) (counter+1)
{-
    Funcion para iterar las filas de una matriz
    (M (l:rest): es la matriz que se quiere iterar
    m: una copia de la matriz para poder obtener los vecinos 
    cuando estamos iterando por cada nodo.
    counter: contador para indicar en que fila estamos iterando.

    se le pasa a iterCols la copia de la matriz, la fila que se esta iterando y el contador,
    como sabemos que iterCols retorna una lista de strings, su resultado lo concatenamos con
    el resultado de llamar recursivamente a iterRows que se le pasa la copia de la matriz,
    para mantenerla, el resto de filas y el counter + 1
-}

iterCols :: Mapper -> [DataType'] -> Int -> Int -> [String]
iterCols m [] r c       = []
iterCols m (h:rest) r c = toOrientations m h : iterCols m rest r (c+1)
{-
    Funcion para iterar las columnas
    m: copia de la matriz
    (h:rest): fila que estamos iterando
    r: fila que estamos iterando
    c: columna que estamos iterando

    Por cada elemento de la fila llamamos a toOrientations
    y concatenamos su resultado con la llamada recursiva de iterCols
-}

toOrientations :: Mapper -> DataType' -> String
toOrientations m node = getOrientation node neighbor
                    where
                        neighbors2 = near node m
                        m_sin_node = set (x, y) (Node "" 0 0) neighbors2
                        neighbor   = irows (getMatrix neighbors2)
                        (x, y)     = (\(Node _ r c) -> (r, c)) node
{-
    Funcion para convertir un nodo a una string con su posicion relativa
    en base a puntos cardinales
    m: copia de la matriz que se esta iterando
    node: nodo a convertir

    neighbors2 = near node m
    obtenemos los nodos que estan alrededor de node en la matriz m

    m_sin_node = set (x, y) (Node "" 0 0) neighbors2
    de la matriz que obtuvimos antes con los nodos cercanos
    eliminamos el nodo que queremos convertir. NO SE USA

    (x, y)     = (\(Node _ r c) -> (r, c)) node
    funcion lambda para obtener las coordenadas del nodo,
    esto para poder usar m_sin_node, pero no se usa.

    neighbor   = irows (getMatrix neighbors2)
    neighbor es cualquier nodo que no este vacio cercano a el nodo que
    queremos convertir

    finalmente llamamos a getOrientations con el nodo a convertir y
    el vecino que encontramos, para construir la string de puntos
    cardinales
-}


{-
    Funciones auxiliares para iterar los nodos cercanos (vecinos)
-}

irows :: [[DataType']] -> DataType'
irows []       = Node "" 0 0
irows (l:rest) = if neighbor == Node "" 0 0 then irows rest else neighbor
                    where
                        neighbor = icols l
{-
    funcion para iterar la matriz de los vecinos
    buscamos en la fila algun nodo, esto lo hacemos llamando a icols con la fila
    si encontramos algun nodo, es decir, si neighbor es distinto de Node "" 0 0
    entonces retornamos ese nodo que encontramos
    caso contrario seguimos iterando las filas.
-}

icols :: [DataType'] -> DataType'
icols [] = Node "" 0 0
icols (h:rest) = if h == Node "" 0 0 then icols rest
                    else h
{-
    funcion para iterar las filas de la matriz de vecinos
    chequeamos cada nodo de la fila buscando algun nodo
    que no este vacio
-}

getOrientation :: DataType' -> DataType' -> String
getOrientation (Node id y x) (Node id2 y2 x2)
    | id == "" || id2 == "" = ""
    | x == x2 && y == y2 = format "\\Vertex[x=%, y=%] {%}" [show x2, show y2,id2]
    | x == x2 && y < y2  = format "\\NO(%) {%}" [id2, id]
    | x == x2 && y > y2  = format "\\SO(%) {%}" [id2, id]
    | x > x2 && y == y2  = format "\\EA(%) {%}" [id2, id]
    | x < x2 && y > y2   = format "\\NOEA(%) {%}" [id2, id]
    | x > x2 && y > y2   = format "\\SOEA(%) {%}" [id2, id]
    | x < x2 && y == y2  = format "\\WE(%) {%}" [id2, id]
    | x > x2 && y < y2   = format "\\NOWE(%) {%}" [id2, id]
    | x < x2 && y < y2   = format "\\SOWE(%) {%}" [id2, id]
    -- 0 0
    -- 0 1
{-
    Funcion para construir la string de posiciones cardinales
    (Node id y x): nodo a convertir
    (Node id2 y2 x2): nodo vecino encontrado

    lo que hacemos es comparar las x y las y. en base a estas podemos determinar
    en que posicion relativa esta un nodo del otro

    con las y podemos determinar NORTE SUR

    y con las x podemos determinar ESTE OESTE

    la combinacion de estas determina SURESTE SUROESTE, NORESTE Y NOROESTE

-}
-- toOrientations' :: Mapper -> (Int, Int) -> String
-- toOrientations' (M (l:rest)) (r, c) = 
test1 = build (Node "" 0 0) 3

test2 = insert (Node "n1" 0 0) test1

test3 = insert (Node "n2" 0 1) test2 

test4 = insert (Node "n3" 1 0) test3

test5 = insert (Node "n4" 1 1) test4

test6 = insert (Node "n5" 2 2) test5

test1_1 = insert (Node "n2" 0 1) (insert (Node "n1" 0 0) test1)