module Main where

import System.Console.Haskeline

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main :: IO ()
main = runInputT defaultSettings loop
   where
       loop :: InputT IO ()
       loop = do
           minput <- getInputLine "> "
           case minput of
               Nothing -> return ()
               Just ".exit" -> return ()
               Just input -> do outputStrLn $ "Input was: " ++ input
                                loop