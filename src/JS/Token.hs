module JS.Token where

data Token = Identifier String 
           | StringLit String 
           | NumLit Double
           | IntLit Int
           | Eq 
           | Period 
           | Comma 
           | LP 
           | RP 
           | LBrack
           | RBrack
           | Add
           | Sub
           deriving Show

mkStringLit :: String -> Token 
mkStringLit = StringLit . init . tail