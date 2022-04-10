module Main where
import System.Environment (getArgs)
import Ast
import Parser
import Evaluador
import Data.Data (Data, DataType)
import Text.Parsec ( runParsecT, modifyState, putState, runParser )
import Text.Parsec.Prim (runParserT)
import Control.Monad.Identity (runIdentity)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.State (runStateT)
import Control.Exception (catch, IOException)
import Data.Char (toLower)
---------------------------------------------------------

main :: IO ()
main = catch (do arg:_ <- getArgs       -- Obtenemos los argumentos de linea de comandos
                 run arg True) handler
                 where
                   handler :: IOException -> IO ()    -- Handler para las excepciones de sistema
                   handler ex = do args <- getArgs
                                   if null args then putStrLn "Usage: lscomp [filepath (.ls)]"
                                   else print ex

-- Ejecuta un programa a partir de su archivo fuente
{- El argumento test es meramente para testing
    test: True -> Parseo y Evaluacion
    test: False -> Parseo sin evaluacion (Imprime el AST obtenido)
-}
run :: [Char] -> Bool -> IO ()
run ifile test = do s <- readFile ifile     -- Lectura del archivo con el codigo fuente
                    case runParser cmdparse initParser ifile s of     -- Ejecutamos el parser
                        Left error -> print error
                        Right t -> do if test then do a <- runStateT (runExceptT (eval t)) []     -- Ejecutamos el evaluador
                                                      case fst a of
                                                        Left e    -> putStrLn e
                                                        Right res -> return ()
                                      else print t

