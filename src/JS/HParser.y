{
module JS.HParser (parser) where 

import Data.Char
import qualified JS.Exp as E
import qualified JS.Token as T
}

-- x = y(z)

{- parser :: [T.Token] -> [E.Expr] -}
%name parser
%tokentype { T.Token }
%error { parseError }

%token 
  identifier  { T.Identifier $$ }
  string      { T.StringLit $$  }
  number      { T.IntLit $$     }
  '='         { T.Eq            }
  '.'         { T.Period        }
  ','         { T.Comma         }
  '('         { T.LP            }
  ')'         { T.RP            }
  '['         { T.LBrack        }
  ']'         { T.RBrack        }

%% 

exps :: { [E.Exp] }    
exps : {- empty -}            { [] }
     | exp exps               { $1 : $2 }

exp :: { E.Exp }
exp : string                    { E.StringLit $1    }
    | exp '=' exp               { E.Assign $1 $3    }
    | identifier '[' number ']' { E.ArrAccess $1 $3 }
    | varaccess                 { $1                }
    | varaccess '(' args ')'    { E.Call $1 $3      }


varaccess :: { E.Exp }    
varaccess : identifier                { E.Var $1 }
          | identifier '.' identifier { E.VarAccess $1 $3 }

args :: { [E.Exp] }     
args : {- empty -}          { []      }
     | exp                  { [$1]    }
     | exp ',' args         { $1 : $3 }

{
parseError :: [T.Token] -> a  
parseError tokens = error $ "Parse Error: " ++ show tokens
}

