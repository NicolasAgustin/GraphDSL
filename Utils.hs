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

flattenLines :: [String] -> String
flattenLines = concat

replace :: String -> (Char, String) -> String 
replace [] _            = []
replace (h:rest) (d, s) = if h == d then s ++ rest else h : replace rest (d, s)   

joinLines :: [String] -> Char -> String 
joinLines [] _       = ""
joinLines (h:rest) c = h ++ [c] ++ joinLines rest c

format :: String -> [String] -> String
format str [] = str
format str (h:rest) = format result_string rest
                                        where
                                            result_string = formatString h str

-- Recibe una string con {} y pone la string 1 en la posicion de {}
formatString :: String -> String -> String
formatString str []        = ""
formatString toPlace (h:rest) = if h == '%' then toPlace ++ rest
                                else h : formatString toPlace rest

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

groupByColor :: [(String, String)] -> [[(String, String)]]
groupByColor [] = []
groupByColor l  = groupBy (\x y -> fst x == fst y) l 