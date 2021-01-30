module JS.Token where

data Token = Identifier String 
           | StringLit String 
           | Eq 
           | Period 
           | Comma 
           | LP 
           | RP 
           deriving Show

mkStringLit :: String -> Token 
mkStringLit = StringLit . init . tail