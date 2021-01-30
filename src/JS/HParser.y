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
  '='         { T.Eq            }
  '.'         { T.Period        }
  ','         { T.Comma         }
  '('         { T.LP            }
  ')'         { T.RP            }

%% 

exps :: { [E.Exp] }    
exps : {- empty -}            { [] }
     | exp exps               { $1 : $2 }

exp :: { E.Exp }
exp : string                  { E.StringLit $1 }
    | '(' exp ')'             { $2 }
    | varaccess               { $1 }
    | varaccess '=' exp       { E.Assign $1 $3 }
    | varaccess '(' args ')'  { E.Call $1 $3   }

varaccess :: { E.Exp }
varaccess : var                      { $1 }
          | var '.' varaccess        { E.VarAccess $1 $3 }

var :: { E.Exp }       
    : identifier            { E.Var $1 }

args :: { [E.Exp] }     
args : {- empty -}          { []      }
     | exp                  { [$1]    }
     | exp ',' args         { $1 : $3 }

{
parseError :: [T.Token] -> a  
parseError tokens = error $ "Parse Error: " ++ show tokens
}

