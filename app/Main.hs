module Main where

import Options.Applicative
import Data.Semigroup ((<>))

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import JavaScript (parseJS, compile)

main :: IO ()
main = do src <- execParser opts
          compileSrc src
  where 
    opts :: ParserInfo String
    opts = info (helper <*> parser) (fullDesc <> progDesc "Compiler SRC into executable TARGET" <> header "jsc - a JavaScript to x86 compiler")

    parser :: Parser String 
    parser = argument str (metavar "SRC" <> help "JavaScript source") 

compileSrc :: String -> IO ()
compileSrc src = do contents <- TIO.readFile src
                    let parsed = parseJS src contents
                    putStrLn $ unlines $ compile parsed

---- greet example

greetMain :: IO ()
greetMain = greet =<< execParser opts
  where
    opts = info (helper <*> sample)
      ( fullDesc
        <> progDesc "Print a greeting for TARGET"
        <> header "hello - a test for optparse-applicative" )

greet :: Sample -> IO ()
greet (Sample h False n) = putStrLn $ "Hello, " ++ h ++ replicate n '!'
greet _ = return ()

data Sample = Sample
  { hello      :: String
  , quiet      :: Bool
  , enthusiasm :: Int }

sample :: Parser Sample
sample = Sample
      <$> strOption
          ( long "hello"
            <> metavar "TARGET"
            <> help "Target for the greeting" )
      <*> switch
          ( long "quiet"
            <> short 'q'
            <> help "Whether to be quiet" )
      <*> option auto
          ( long "enthusiasm"
            <> help "How enthusiastically to greet"
            <> showDefault
            <> value 1
            <> metavar "INT" )