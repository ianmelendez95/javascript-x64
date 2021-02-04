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
  , forLoop
  , variable
  , strLitDbl
  , number
  ]

-- | precedence ref: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Operator_Precedence
-- |   higher number, earlier priority
operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [ postfix "++" incr, postfix "--" decr ] -- 17
  , [ binary "+" Add, binary "-" Sub ]     -- 14
  , [ binary "<" Clt, binary ">" Cgt, binary ">=" Cgte, binary "<=" Clte ] -- 12
  , [ binary "==" Ceq, binary "!=" Cneq ]  -- 11
  , [ binary "=" Assign ]                  -- 3
  ]
  where 
    incr :: Expr -> Expr
    incr e = Assign e (Add e (Num 1))

    decr :: Expr -> Expr 
    decr e = Assign e (Sub e (Num 1))

    binary :: String -> (Expr -> Expr -> Expr) -> Operator Parser Expr
    binary  name f = InfixL  (f <$ symbol name)

    postfix :: String -> (Expr -> Expr) -> Operator Parser Expr
    postfix name f = Postfix (f <$ symbol name)

forLoop :: Parser Expr
forLoop = do (e1, e2, e3) <- symbol "for" *> forTerms 
             For e1 e2 e3 <$> braces (many $ lexeme expr) 
  where 
    forTerms :: Parser (Expr, Expr, Expr)
    forTerms = lexeme $ parens $ (,,) <$> lexeme expr <*> (symbol ";" *> lexeme expr) <*> (symbol ";" *> lexeme expr)

strLitDbl :: Parser Expr
strLitDbl = Str <$> lexeme (char '"' >> manyTill L.charLiteral (char '"'))

strLitSgl :: Parser Expr 
strLitSgl = Str <$> lexeme (char '\'' >> manyTill L.charLiteral (char '\''))

number :: Parser Expr
number = Num <$> lexeme (try L.float <|> L.decimal)

variable :: Parser Expr 
variable = Var <$> lexeme identifier

identifier :: Parser String 
identifier = (:) <$> letterChar <*> many (alphaNumChar <|> char '_')

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

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