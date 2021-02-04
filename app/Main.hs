module Main where

import System.Console.Haskeline

import Control.Monad.State.Lazy
import Control.Monad.Except

import JS.HParser
import qualified JS.NEval as E
import JS.Value
import JS.ALexer
import JS.Environment (Environment (..), initialEnvironment)
import Text.Pretty.Simple (pPrint)

import System.Environment
import Acorn.Client

main :: IO ()
main = do args <- getArgs  
          case args of 
            [] -> runInputT (defaultSettings { historyFile = Just "hjs-history" }) 
                            (loop $ initialEnvironment args)
            (file : _) -> do content <- readFile file
                             parsed <- parseJS content
                             pPrint parsed

loop :: Environment -> InputT IO ()
loop env = 
  do minput <- getInputLine "> "
     case minput of
       Nothing -> return ()
       Just ".exit" -> return ()
       Just input -> do liftIO $ evalJS input
                        loop env

eval :: Environment -> String -> IO (Value, Environment) 
eval env input = do let toks = alexScanTokens input
                    putStrLn $ "Tokens: " ++ show toks
                    let parsed = parser toks
                    putStrLn $ "Syntax: " ++ show parsed
                    result <- liftIO $ runExceptT . runStateT (E.evalExprs parsed) $ env 
                    case result of 
                      (Left err) -> print err >> return (Undefined, env)
                      (Right resVal) -> return resVal

evalJS :: String -> IO ()
evalJS input = do parsed <- parseJS input
                  pPrint parsed