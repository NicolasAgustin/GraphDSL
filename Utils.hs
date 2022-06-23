{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Utils where
import Data.List
import System.IO
    ( hClose, openFile, hPutStrLn, IOMode(AppendMode) )

-- Funcion auxiliar para splitear una cadena de caracteres en un caracter dado
{- Dropea los caracteres hasta llegar a ',' -}
splitOn     :: Char -> String -> [String]
splitOn p s =  case dropWhile (p==) s of
                      "" -> []
                      s' -> w : splitOn p s''
                            where (w, s'') = break (p==) s'
{-
    funcion para poder recortar una string en un caracter especifico
    p: caracter en el que debemos cortar
    s: string que vamos a cortar

    primero dropeamos los caracteres mientras sean iguales al separador
    (para poder eliminar si el separador aparece antes.)
    si la string no es vacia entonces
    obtenemos todos los caracteres hasta encontrar el separador
        break (p==) s'
    break devuelve una tupla, donde el primer componente son todos los elementos hasta 
    el separador y la segunda componente es todo lo que resta.

    concatenamos la primer componente al resultado final 
        w : splitOn p s''
    y llamamos recursivamente a la funcion con el separador y lo que resta por
    dividir.
-}


flattenLines :: [String] -> String
flattenLines = concat
{-
    funcion para aplanar una lista de listas, de string
    se podria usar concat directamente, pero con flattenLines
    nos aseguramos que solo le pasemos strings

    ademas del nombre descriptivo para lo que queremos hacer.

    concat concatena todos los elementos de una lista, donde la lista
    que le pasamos debe ser de elementos que se puedan plegar
    en resumen, listas
-}


{-
    Funcion para reemplazar un caracter en una string por otra string
-}

replace :: String -> (Char, String) -> String 
replace [] _            = []
replace (h:rest) (d, s) = if h == d then s ++ rest else h : replace rest (d, s)   
{-
    funcion para reemplazar un caracter por una nueva string dentro de otra string
    (h:rest): lista src
    (d, s): tupla donde
        d: caracter que queremos reemplazar
        s: string por la que queremos reemplazar el caracter.
-}

{-
    Funcion para unir una lista en un caracter
-}
joinLines :: [String] -> Char -> String 
joinLines [] _       = ""
joinLines (h:rest) c = h ++ [c] ++ joinLines rest c
{-
    funcion para concatenar una lista de listas 
    en un caracter especifico.
    similar a
        ''.join([1,2,3]) en python
    (h:rest): lista que queremos unir, donde cada elemento es una lista
    c: caracter que usaremos para unir
-}

format :: String -> [String] -> String
format str [] = str
format str (h:rest) = format result_string rest
                                        where
                                            result_string = formatString h str
{-
    funcion para formatear una string

    str: string que tendra el %, esta sera la string a formatear

    (h:rest): lista de strings que va a tener los valores que se van a reemplazar en lugar
    de % 

    Vamos reemplazando uno a uno los % que estan en str por cada uno de los elementos
    de la otra lista. Vamos acumulando el resultado en result_string
    en la que progresivamente vamos a ir eliminando los 
    % que existen.
        result_string = formatString h str
    llamamos recursivamente a format con los elementos a reemplazar restantes y 
    la string que vamos acumulando.
-}


-- Recibe una string con % y pone la string 1 en la posicion de %
formatString :: String -> String -> String
formatString str []        = ""
formatString toPlace (h:rest) = if h == '%' then toPlace ++ rest
                                else h : formatString toPlace rest
{-
    funcion que reemplaza el caracter % en una string por otra string dada por el primer argumento
    voy tomando uno a uno los caracteres, si es igual a % entonces saco ese caracter y tomo toPlace
    que tendra el valor por el cual reemplazar y lo concateno al resto
    si no sigo iterando recursivamente
-}


{- 
    Dada una string con posiblemente caracteres de salto de linea, separa dicha string en lineas y por cada una la escribe
    en el archivo especificado por path
-}
append :: String -> String -> IO ()
append path lines = do writeLines path (splitOn '\n' lines)
                        where
                            writeLines fp []   = return ()
                            writeLines fp (line:lines) = do
                                hd <- openFile fp AppendMode
                                hPutStrLn hd line
                                hClose hd
                                writeLines fp lines

{-
    funcion que recibe un path a un archivo y una string con saltos de linea \n
    separa esa string en lineas y escribe una a una en el archivo
    path: path al archivo
    lines: string que contiene \n

    Debido a que estamos trabajando con entrada y salida
    el tipo de retorno de la funcion sera monadico
    usamos la monada IO ()
        append :: String -> String -> IO ()

    primero separamos la string con saltos de linea
        writeLines path (splitOn '\n' lines)
    
    mediante pattern matching si la lista esta vacia retornamos void
        writeLines fp []   = return ()

    sino esta vacia llamamos a openFile dentro de un do
    fp es el path al archivo. Indicamos AppendMode para el modo de apertura
    el resultado de openFile es un handler para el archivo
        hd <- openFile fp AppendMode

    luego iteramos por cada linea
        line: linea que estamos iterando y que debemos escribir
        hd: handler del archivo
        hPutStrLn imprime una string a un archivo y agrega un salto de linea al final.
        hPutStrLn hd line

    cerramos el archivo con el handler
        hClose hd

    llamamos recursivamente a la funcion para seguir iterando por las lineas
        writeLines fp lines

-}

groupByColor :: [(String, String)] -> [[(String, String)]]
groupByColor [] = []
groupByColor l  = groupBy (\x y -> fst x == fst y) l 
{-
    funcion para agrupar los nodos con colores
    que estan representados con 
    (string, string) -> (color, id nodo)
    groupBy toma una funcion con la que le indicamos como comparar los elementos
    debe ser un predicado, es decir, devolver un valor booleano
    y toma una lista donde estaran los elementos sin agrupar

    el resultado de groupBy es una lista donde cada elemento sera 
    un grupo encontrado.

    groupBy (\x y -> fst x == fst y) [("red", 1), ("red", 2), ("blue", 2)]

    retorna:
        [ 
            [("red",1),("red",2)],  # grupo de nodos rojos
            [("blue",2)]            # grupo de nodos azules
        ]

-}