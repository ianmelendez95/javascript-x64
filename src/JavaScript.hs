{-# LANGUAGE OverloadedStrings #-}

module JavaScript where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Void

type Parser = Parsec Void Text

data JS = JConsoleLog String deriving (Show)

compile :: JS -> [String]
compile (JConsoleLog str) = 
  [ "          global _start"
  , ""
  , "          section   .text"
  , "_start:   mov       rax, 1"
  , "          mov       rdi, 1"
  , "          mov       rsi, message"
  , "          mov       rdx, " ++ show (length str + 1)
  , "          syscall"
  , "          mov       rax, 60"
  , "          xor       rdi, rdi"
  , "          syscall"
  , ""
  , "          section   .data"
  , "message:  db        \"" ++ str ++ "\", 10" 
  ]

parseJS :: FilePath -> Text -> JS
parseJS fileName content = case parse pConsoleLog fileName content of 
                            (Left err) -> error $ errorBundlePretty err
                            (Right res) -> res

pConsoleLog :: Parser JS
pConsoleLog = JConsoleLog . T.unpack 
  <$> (string "console.log" *> between (char '(') (char ')') pStringLiteral)
                 
   
pStringLiteral :: Parser Text 
pStringLiteral = char '"' >> takeWhileP (Just "string literal") (/= '"') <* char '"'