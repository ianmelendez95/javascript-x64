module Main where

import System.Console.Haskeline

import Text.Megaparsec hiding (ParseError)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import JS.Syntax
import JS.Parser

main :: IO ()
main = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine "> "
      case minput of
        Nothing -> return ()
        Just ".exit" -> return ()
        Just input -> do printParsed $ parseExpr input
                         loop
    printParsed :: Either ParseError Expr -> InputT IO ()
    printParsed (Left err) = outputStrLn $ errorBundlePretty err
    printParsed (Right expr) = outputStrLn $ show expr
    
