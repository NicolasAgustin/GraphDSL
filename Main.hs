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
import Control.Error (runExceptT)
import Conduit (MonadIO(liftIO))
import Control.Monad.Trans.State (runStateT)
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
                Right t    -> do a <- runStateT (runExceptT (eval t)) []
                                 case fst a of
                                     Left e    -> putStrLn e
                                     Right res -> return ()
