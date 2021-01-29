module JS.Syntax where

data Expr = Num Double
          | Str String 
          | Var String
          | Add Expr Expr 
          | Sub Expr Expr
          | For Expr Expr Expr [Expr]
          | Assign Expr Expr
          deriving (Show, Eq)
