--{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use drop" #-}
{-# HLINT ignore "Use take" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Matriz where
import Data.Data
import Data.List (elemIndex)

newtype Matrix a = M [[a]] deriving (Eq, Data)
{-
    newtype es mas eficiente que datatype, debido a que el compilador
    sabe que se va a utilizar un nuevo tipo para envolver un tipo preexistente
    debido a esto puede manejar mas eficazmente los wrapping y unwrapings

    newtype es para hacer un tipo completamente nuevo a partir de un tipo existente,
    implica menos sobrecarga al compilador, ya que no tiene que evaluar el tipo, porque
    al tener un solo constructor ya sabe de que tipo sera.
-}

type Coord = (Integer, Integer)
{-
    tipo coordenada, para poder representar coordenadas
-}

instance (Show a) => Show (Matrix a) where
    show (M [])       = ""
    show (M (f:rest)) = "| " ++ r f ++ "\n" ++ show (M rest)
        where
            r l = foldr ((\x y -> x ++ " | " ++ y) . show) "" l
{-
    Instancia para mostrar una matriz, la matriz se muestra de la siguiente
    manera:
    | x | x |
    | x | x |
-}

make :: Show a => [a] -> [String]
make = map show
{-
    castea todos los elementos de una lista en string
-}

getMatrix :: Matrix a -> [[a]]
getMatrix (M m) = m

empty :: Matrix a
empty = M [[]]

{-
    Construimos una matrix de n*n populandola con
    el valor e
-}
build :: a -> Integer -> Matrix a
build e 0 = M [[]]
build e n = M $ replicate (fromIntegral n) (replicate (fromIntegral n) e)
-- fromIntegral :: Integer -> Int

{- Funcion para obtener las dimensiones de una matriz -}
dimensions :: Matrix a -> (Integer, Integer)
dimensions (M [])         = (0, 0)
dimensions (M m@(l:rest)) = (toInteger $ length m - 1, toInteger $ length l - 1)
{-
    toInteger :: Int -> Integer
    (obtenemos la cantidad de filas - 1) (Obtenemos la cantidad de columnas - 1)
    Como siempre las matrices van a ser cuadradas podemos asumir que la cantidad de columnas es
    igual en todas las filas.
-}

get :: Coord -> Matrix a -> a
get (row, col) = get' (fromIntegral row, fromIntegral col)

get' :: (Int, Int) -> Matrix a -> a
get' (row, col) (M []) = error "Index out of range"
get' (row, col) (M f)  = (f !! row) !! col
-- (!!) :: [a] -> Int -> a
{-
    A esta funcion le pasamos dos coordenadas y devuelve el elemento en esa posicion
    primero obtenemos la fila (f!!row) y el resultado de eso, es decir, la fila
    obtenemos la columna dentro de esa fila
-}

set :: Coord -> a -> Matrix a -> Matrix a
set (row, col) = set' (fromIntegral row, fromIntegral col)

set' :: (Int, Int) -> a -> Matrix a -> Matrix a
set' (row, col) e (M []) = M [[e]]
set' (row, col) e (M m)  = M (above ++ [new_row] ++ below)
                where
                    entire_row     = m !! row
                    (left, right)  = (take col entire_row, drop (col+1) entire_row)
                    new_row        = left ++ [e] ++ right
                    (above, below) = (take row m, drop (row+1) m)
{-
    Esta funcion recibe una coordenada y un elemento, procedemos a insertar el elemento dentro
    de esa posicion de la matriz.
    entire_row: es la row de la matriz que especifica la primer coordenada, es decir (row, )
    (left, right): 
        left es una lista con todos los elementos a la izquierda del elemento especificado por las coordenadas.
        la funcion take toma los primeros n elementos de una lista. Esto funciona porque si tenemos las coordenadas
        (1,1) y una fila [1,2,3]: take 1 [1,2,3] tomara [1] sin tener en cuenta el 2 que esta en la posicion que queremos
        modificar. 
        ejemplo: [1,2,3,n,4,5,6] -> left = [1,2,3]
        right es una lista con todos los elementos de la derecha. ejemplo: [1,2,3,n,4,5,6] -> right = [4,5,6]
        drop dropea los primeros n elementos, debido a que no queremos tener en cuenta el elemento que esta en la 
        posicion que nos interesa, usamos col+1 
    (above, below): utilizamos el mismo procedimiento que para left y right
        above seran las filas de arriba y below las filas de abajo
    new_row: es la fila con el elemento nuevo insertado, por eso debemos agregarle left y right a ambos lados y colocar el elemento en el medio
-}


mmap :: (a -> b) -> Matrix a -> Matrix b
mmap _ (M [])       = M []
mmap f (M (l:rest)) = M $ map f l : getMatrix (mmap f (M rest))
{- 
    funcion para aplicar un map en una matrix
-}

search :: (Eq a) => a -> Matrix a -> Coord
search e m = search' e m (0, 0) (fst $ dimensions m)
{-
    Busca un elemento e en la matriz. 
    e: elemento a buscar
    m: matriz en la que debemos buscar.
    Con estos parametros llamamos a la funcion auxiliar
    search' ->
            (0, 0): es la coordenada en la que empezamos a buscar.
            Siempre va a ser (0, 0) debido a que comenzamos a buscar desde el principio
            (fst $ dimensions m): es el limite de la matriz. Como nuestra matriz siempre
            va a ser cuadrada, podemos fiarnos de cualquiera de las dos coordenadas que retorne
            dimensions.
-}

search' :: (Eq a) => a -> Matrix a -> Coord -> Integer -> Coord
search' _ (M []) (r, c) rs       = (r, c)
search' e (M (l:rest)) (r, c) rs = if b && r <= rs then (r, i) else search' e (M rest) (r+1, c) rs
                                            where
                                                (b, i) = case elemIndex e l of
                                                            Nothing -> (False, 0)
                                                            Just i  -> (True, toInteger i)

{-
    Esta funcion busca un elemento en la matriz.

    e: elemento a buscar
    (M (l:rest)): matriz en la que debemos buscar. l es una fila rest son las demas filas.
    (r, c): coordenada en la cual empezar a buscar. (siempre va a ser (0,0))
    rs: limite de la matriz (primer componente de dimensions)

    El algoritmo que utiliza la funcion es el siguiente:
    Buscamos el indice de la fila en el que esta el elemento que buscamos,
    para esto usamos elemIndex :: Eq a => a -> [a] -> Maybe Int
    Si es nothing, el elemento no esta en la fila. Entonces llamamos recursivamente a search'
    con el mismo elemento, el resto de las filas (rest), r+1 porque tenemos que buscar en la siguiente
    fila, con c no hacemos nada y rs porque los limites van a ser los mismos.
    Si es just entonces el elemento esta presente en la fila. devolvemos (true, index) donde index es la
    columna donde esta el elemento en esa fila.
    Una vez que encontramos el elemento, debemos chequear si todavia no nos pasamos del limite.
    Si no nos pasamos, entonces devolvemos (r, i) donde r es la fila en donde encontramos el elemento e i es 
    la columna donde lo encontramos.

    if b && r <= rs then (r, i) else search' e (M rest) (r+1, c) rs
        where
            (b, i) = case elemIndex e l of
                        Nothing -> (False, 0)
                        Just i  -> (True, toInteger i)
-}

firstCoord :: Coord -> Coord 
firstCoord (x, y) = (x1, y1)
                    where 
                        x1 = if x-1 < 0 then x else x-1
                        y1 = if y-1 < 0 then y else y-1
{-
    Esta funcion sirve para obtener la punta izquierda superior en base a un punto
    para construir el cuadrado de los puntos que estar al rededor de un n 
    Conseguimos el punto izquierdo relativo a n:
            x - -
            - n -
            - - - 

-}


secondCoord :: Coord -> (Int, Int) -> Coord 
secondCoord (x, y) (lx, ly) = (x1, y1)
                                where 
                                    x1 = if x+1 == toInteger lx then x else x+1
                                    y1 = if y+1 == toInteger ly then y else y+1

{-
    Esta funcion sirve para obtener la punta derecha inferior en base a un punto
    para construir el cuadrado de los puntos que estar al rededor de un n 
    Conseguimos el punto derecho relativo a n:
            - - -
            - n -
            - - x

    En este caso tambien debemos pasarle un segundo parametro que seran las dimensiones de la matriz,
    para poder detectar cuando nos estamos pasando del limite.
-}


near :: (Eq a) => a -> Matrix a -> Matrix a
near e (M []) = M []
near e ma@(M (l:rest)) = neighbors (fp, sp) ma
                            where 
                                scoord = search e ma
                                fp = firstCoord scoord 
                                sp = secondCoord scoord (length l, length (l:rest)) 

{-
    Esta funcion recibe un elemento y retorna todos los elementos que estan a su alrededor en la matriz
    scoord: es la coordenada en donde esta el elemento e en la matriz ma
    fp: es la punta superior izquierda de scoord
        x   -    -
        - scoord -
        -   -    -
    sp: es la punta inferior derecha de scoord. La obtenemos con secondCoord, 
    que recibe las dimensiones de la matriz
        -   -    -
        - scoord -
        -   -    x
    Se podria haber hecho con dimensions pero resulto mas sencillo en el momento con length
-}

fst' :: (a, a) -> a
fst' (x,_) = x

sublist :: (Eq a) => Integer -> Integer -> [a] -> [a]
sublist from to l = if from == to then [l !! fromIntegral from] else (l !! fromIntegral from) : sublist (from+1) to l
{-
    Esta funcion es para obtener una sublista de una lista
    from es el indice inicial, to es el indice maximo

    el algoritmo que seguimos es: 
        si from es igual a to es decir los dos indices son iguales
        entonces devolvemos la posicion indicada por from (es indistinto cualquiera de los dos porque son iguales)
        si no son iguales obtenemos el elemento indicado por from y llamamos recursivamente a sublist con from + 1
        de esta forma vamos incrementando from hasta llegar a to en ese momento se termina la recursion y devuelve el
        elemento en esa posicion.
-}


neighbors :: (Eq a) => (Coord, Coord) -> Matrix a -> Matrix a
neighbors (r, c) (M []) = M []
neighbors (r, c) (M m)  = M new_columns
                                where
                                    new_rows   = sublist (fst r) (fst c) m
                                    new_columns = map (sublist (snd r) (snd c)) new_rows

{-
    Esta funcion recibe una tupla de coordenadas (punta sup izq y punta inf derecha):
    x - -
    - n -
    - - x
    Y la matriz que tiene que recortar
    x x x -
    x x x -
    x x x -
    - - - -
    new_rows: es una lista de listas con las filas que va a tener la matriz recortada.
    le pasa a sublist las primeras componentes de ambos puntos 
    (x, _) (y, _)
    estas componentes indican las filas. desde la fila x hasta la fila y.
    entonces recortamos la matriz con sublist en base a estos indices.

    new_columns: realizamos el mismo procedimiento que para las filas pero ahora para las columnas.
    Para esto tenemos que aplicar la funcion sublist, con la segunda componente de cada coordenada
    osea, (_, x) (_, y) cada una de estas componentes indican la cantidad de columnas que tendra la nueva matriz
    y hacemos un mapeo de la funcion por cada fila de new_rows.

    finalmente new_columns tendra la matriz recortada.

-}

calculateLimit :: Coord -> Coord -> (Coord, Coord)
calculateLimit (ri, ci) (rs, cs) = ((r_upper, c_upper), (r_lower, c_lower))
                                where
                                    r_upper = if (ri-1) >= 0 then ri-1 else ri
                                    c_upper = if (ci-1) >= 0 then ci-1 else ci
                                    r_lower = if (ri+1) < rs then ri+1 else ri
                                    c_lower = if (ci+1) < cs then ci+1 else ci

{-
    Esta funcion calcula los puntos izquierdos y superiores en base a una coordenada
    recibe la coordenada especifica y las dimensiones. No se usa
-}

testt = set (1,1) 1 (build 0 3)
