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

main :: IO ()
main = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine "> "
      case minput of
        Nothing -> return ()
        Just ".exit" -> return ()
        Just input -> 
          case parseExpr input of 
            (Left err) -> outputStrLn (errorBundlePretty err) >> loop
            (Right parsed) -> 
              do outputStrLn $ "Parsed: " ++ show parsed
                 evalResult <- liftIO $ evalStart parsed
                 case evalResult of 
                   (Left err) -> outputStrLn ("JS Error: " ++ show err) >> loop
                   (Right res) -> outputStrLn (show res) >> loop

    printParsed :: Either ParseError Expr -> InputT IO ()
    printParsed (Left err) = outputStrLn $ errorBundlePretty err
    printParsed (Right expr) = outputStrLn $ show expr

    evalStart :: Expr -> IO (Either JSError Value)
    evalStart expr = runExceptT . evalStateT (eval expr) $ emptyEnvironment