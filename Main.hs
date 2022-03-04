module Main where

import System.Environment (getArgs)

import Ast
import Parser
import Evaluador
import Data.Data (Data, DataType)
import Data.Set
import Text.Parsec ( runParsecT, modifyState, putState, runParser )
import Text.Parsec.Prim (runParserT)
import Control.Monad.Identity (runIdentity)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.State (runStateT)
import Control.Exception (catch, IOException)
import Data.Char (toLower)
---------------------------------------------------------

-- Hay que cambiar la implementacion del parser porque el chequeo de estado dificulta la evaluacion posterior

type ParseState = [(String, Types)]

initParserState :: ParseState
initParserState = []

main :: IO ()
main = catch (do arg:_ <- getArgs
                 run arg True) handler
                 where
                   handler :: IOException -> IO ()
                   handler ex = do putStrLn (show ex)
                                   putStrLn "Usage: lscomp [filepath (.ls)]"


-- Ejecuta un programa a partir de su archivo fuente
run :: [Char] -> Bool -> IO ()
run ifile test = do s <- readFile ifile
                    case runParser cmdparse initParser ifile s of
                        Left error -> print error
                        Right t -> do if test then do a <- runStateT (runExceptT (eval t)) []
                                                      case fst a of
                                                        Left e    -> putStrLn e
                                                        Right res -> return ()
                                      else print t

