module Main where

import System.Console.Haskeline

import Text.Megaparsec hiding (ParseError)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Control.Monad.State.Lazy
import Control.Monad.Except

import JS.Syntax
import JS.Parser
import JS.Eval
import JS.Value
import JS.Runtime
import JS.ALexer

main :: IO ()
main = runInputT defaultSettings (loop emptyEnvironment)

loop :: Environment -> InputT IO ()
loop env = 
  do minput <- getInputLine "> "
     case minput of
       Nothing -> return ()
       Just ".exit" -> return ()
       Just input -> do let tokens = alexScanTokens input
                        liftIO $ print tokens
                        loop env
        --  case parseExpr input of 
        --    (Left err) -> outputStrLn (errorBundlePretty err) >> loop env
        --    (Right parsed) -> 
        --      do outputStrLn $ "Parsed: " ++ show parsed
        --         evalResult <- liftIO $ evalStart parsed
        --         case evalResult of 
        --           (Left err) -> outputStrLn ("JS Error: " ++ show err) >> loop env
        --           (Right (res, env')) -> outputStrLn (show res) >> loop env'
  where 
    printParsed :: Either ParseError Expr -> InputT IO ()
    printParsed (Left err) = outputStrLn $ errorBundlePretty err
    printParsed (Right expr) = outputStrLn $ show expr

    evalStart :: Expr -> IO (Either JSError (Value, Environment))
    evalStart expr = runExceptT . runStateT (eval expr) $ env