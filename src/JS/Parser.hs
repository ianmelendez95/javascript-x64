{-# LANGUAGE OverloadedStrings #-}

module JS.Parser where 

import Text.Megaparsec hiding (ParseError)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr
import Data.Void
import Data.Char

import JS.Syntax

type Parser = Parsec Void String
type ParseError = ParseErrorBundle String Void

parseExpr :: String -> Either ParseError Expr
parseExpr  = parse expr "<stdio>"

expr :: Parser Expr
expr = makeExprParser term operatorTable

term :: Parser Expr
term = choice
  [ parens expr
  , variable
  , stringLiteral
  , number
  ]

operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [ binary "+" Add, binary "-" Sub ]
  , [ binary "=" Assign ] ]
  where 
    binary :: String -> (Expr -> Expr -> Expr) -> Operator Parser Expr
    binary  name f = InfixL  (f <$ symbol name)

    prefix, postfix :: String -> (Expr -> Expr) -> Operator Parser Expr
    prefix  name f = Prefix  (f <$ symbol name)
    postfix name f = Postfix (f <$ symbol name)

stringLiteral :: Parser Expr
stringLiteral = Str <$> lexeme (char '"' >> manyTill L.charLiteral (char '"'))

number :: Parser Expr
number = Num <$> lexeme (try L.float <|> L.decimal)

variable :: Parser Expr 
variable = Var <$> lexeme identifier

identifier :: Parser String 
identifier = (:) <$> letterChar <*> many (alphaNumChar <|> char '_')

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

sc :: Parser ()
sc = L.space
  space1                      
  (L.skipLineComment "//")      
  (L.skipBlockComment "/*" "*/")

hspace1 :: Parser ()
hspace1 = () <$ takeWhile1P (Just "white space") isHSpace

isHSpace :: Char -> Bool
isHSpace x = isSpace x && x /= '\n' && x /= '\r'