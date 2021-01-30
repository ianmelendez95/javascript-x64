module JS.Exp where 

-- | expression evaluates to a value
data Exp = StringLit String
         | Var String
         | VarAccess Exp Exp
         | Call Exp [Exp]
         | Assign Exp Exp
         deriving Show

-- | var access evaluates to a slot in the environment
-- data VarAccess = Var String 
--                | Acc VarAccess VarAccess
--                deriving Show