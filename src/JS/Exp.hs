module JS.Exp where 

data Exp = StringLit String
         | Call Exp [Exp]
         | Var String
         | ObjAcc Exp Exp
         | Assign Exp Exp
         deriving Show