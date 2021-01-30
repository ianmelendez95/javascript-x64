module JS.Lexer where


import Text.Megaparsec hiding (ParseError, Token, token)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr
import Data.Void
import Data.Char
import Data.Functor

import JS.Token

type Parser = Parsec Void String
type ParseError = ParseErrorBundle String Void

tokenize :: String -> Either ParseError [Token]
tokenize = parse (many token) "<stdio>"

token :: Parser Token
token = choice 
  [ strLit
  , eq
  , period
  , comma 
  , leftP
  , rightP
  , identifier
  ]

strLit :: Parser Token 
strLit = try strLitDbl <|> strLitSgl

strLitDbl :: Parser Token
strLitDbl = StringLit <$> lexeme (char '"' >> manyTill L.charLiteral (char '"'))

strLitSgl :: Parser Token 
strLitSgl = StringLit <$> lexeme (char '\'' >> manyTill L.charLiteral (char '\''))

-- number :: Parser Token
-- number = Num <$> lexeme (try L.float <|> L.decimal)

-- variable :: Parser Token 
-- variable = Var <$> lexeme identifier

identifier :: Parser Token 
identifier = Identifier <$> lexeme ((:) <$> letterChar <*> many (alphaNumChar <|> char '_'))

eq :: Parser Token 
eq = symbol "=" $> Eq

period :: Parser Token 
period = symbol "." $> Period

comma :: Parser Token 
comma = symbol "," $> Comma

leftP :: Parser Token
leftP = symbol "(" $> LP

rightP :: Parser Token
rightP = symbol ")" $> RP

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