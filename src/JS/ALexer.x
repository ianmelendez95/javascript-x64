{
module JS.ALexer (alexScanTokens) where

import qualified JS.Token as T
}

%wrapper "basic"

@id    = [a-zA-Z][a-zA-Z0-9]*

tokens :- 
  $white+               ;
  \;                    ;
  =                     { \_ -> T.Eq                }
  \.                    { \_ -> T.Period            }
  \,                    { \_ -> T.Comma             }
  \(                    { \_ -> T.LP                }
  \)                    { \_ -> T.RP                }
  [a-zA-Z][a-zA-Z0-9]*  { \id -> T.Identifier id    }
  \" [^\"]* \"          { \str -> T.mkStringLit str }
  \' [^\']* \'          { \str -> T.mkStringLit str }

