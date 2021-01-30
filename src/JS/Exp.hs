module JS.Exp where 

data Exp = StringLit String
         | Call String [Exp]
         | Assign String Exp
         deriving Show