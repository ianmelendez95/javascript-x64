module Main where

import System.Console.Haskeline

import Text.Megaparsec hiding (ParseError)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Control.Monad.State.Lazy
import Control.Monad.Except
import Control.Monad (void)

import JS.HParser
import qualified JS.NEval as E
import JS.Value
import JS.Runtime
import JS.ALexer
import JS.RunState
import JS.Exp (Exp)
import JS.Environment (Environment (..), initialEnvironment)

import System.Environment

main :: IO ()
main = do args <- getArgs  
          case args of 
            [] -> runInputT defaultSettings (loop initialEnvironment )
            [file] -> do content <- readFile file
                         void $ eval initialEnvironment content

loop :: Environment -> InputT IO ()
loop env = 
  do minput <- getInputLine "> "
     case minput of
       Nothing -> return ()
       Just ".exit" -> return ()
       Just input -> do env' <- liftIO $ eval env input
                        loop env'

eval :: Environment -> String -> IO Environment 
eval env input = do let tokens = alexScanTokens input
                    putStrLn $ "Tokens: " ++ show tokens
                    let parsed = parser tokens
                    putStrLn $ "Syntax: " ++ show parsed
                    result <- liftIO $ runExceptT . runStateT (E.evalExprs parsed) $ env 
                    case result of 
                      (Left err) -> print err >> return env
                      (Right (val, env')) -> print val >> return env'