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
import JS.Environment (Environment (..), emptyEnvironment, initialEnvironment)

import System.Environment


main :: IO ()
main = do args <- getArgs  
          case args of 
            [] -> runInputT defaultSettings (loop emptyEnvironment)
            [file] -> do content <- readFile file
                         void $ eval content

loop :: Environment -> InputT IO ()
loop env = 
  do minput <- getInputLine "> "
     case minput of
       Nothing -> return ()
       Just ".exit" -> return ()
       Just input -> do liftIO $ eval input
                        loop env
        --  case parseExpr input of 
        --    (Left err) -> outputStrLn (errorBundlePretty err) >> loop env
        --    (Right parsed) -> 
        --      do outputStrLn $ "Parsed: " ++ show parsed
        --         evalResult <- liftIO $ evalStart parsed
        --         case evalResult of 
        --           (Left err) -> outputStrLn ("JS Error: " ++ show err) >> loop env
        --           (Right (res, env')) -> outputStrLn (show res) >> loop env'

eval :: String -> IO () 
eval input = do let tokens = alexScanTokens input
                putStrLn $ "Tokens: " ++ show tokens
                let parsed = parser tokens
                putStrLn $ "Syntax: " ++ show parsed
                result <- runExceptT . evalStateT (E.evalExprs parsed) $ initialEnvironment 
                print result 
                return ()