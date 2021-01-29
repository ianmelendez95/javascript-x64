module JS.Syntax where

data Expr = Num Double
          | Str String 
          | Var String
          | Clt  Expr Expr
          | Cgt  Expr Expr 
          | Ceq  Expr Expr 
          | Clte Expr Expr
          | Cgte Expr Expr
          | Cneq Expr Expr
          | Add  Expr Expr 
          | Sub  Expr Expr
          | Assign Expr Expr
          | For  Expr Expr Expr [Expr]
          deriving (Show, Eq)
