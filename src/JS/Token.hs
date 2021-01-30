module JS.Token where

data Token = Identifier String 
           | StringLit String 
           | NumLit Double
           | Eq 
           | Period 
           | Comma 
           | LP 
           | RP 
           | Add
           | Sub
           deriving Show

mkStringLit :: String -> Token 
mkStringLit = StringLit . init . tail