module Main where

import System.Environment (getArgs)

import Ast
import Parser
import Evaluador
import Data.Data (Data, DataType)
import Data.Set
import Text.Parsec (runParsecT, modifyState, putState)
import Text.Parsec.Prim (runParserT)
import Control.Monad.Identity (runIdentity)
import Text.Parsec (runParser)
---------------------------------------------------------

type ParseState = [(String, Types)]

initParserState :: ParseState 
initParserState = []

main :: IO ()
main = do arg:_ <- getArgs
          run arg

-- Ejecuta un programa a partir de su archivo fuente
run :: [Char] -> IO ()
run ifile = do s <- readFile ifile
               case runParser cmdparse [] ifile s of
                Left error -> print error
                -- Right t    -> print t
                Right t    -> case eval t of 
                                Left err -> print err 
                                Right r  -> print r
    --   Right t    -> print (eval t) --imprimir el resultado de evaluar.
      --Right t    -> print t        --imprimir sin evaluar (para testear Parser)
