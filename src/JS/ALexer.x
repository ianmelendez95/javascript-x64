{
module JS.ALexer (alexScanTokens) where

import qualified JS.Token as T
}

%wrapper "basic"

@id    = [a-zA-Z][a-zA-Z0-9]*

tokens :- 
  $white+                               ;
  \;                                    ;
  =                                     { \_ -> T.Eq                  }
  \.                                    { \_ -> T.Period              }
  \,                                    { \_ -> T.Comma               }
  \(                                    { \_ -> T.LP                  }
  \)                                    { \_ -> T.RP                  }
  \+                                    { \_ -> T.Add                 } 
  \-                                    { \_ -> T.Sub                 } 
  \[                                    { \_ -> T.LBrack              }
  \]                                    { \_ -> T.RBrack              }
  [0-9]+                                { \num -> T.IntLit (read num) }
  [0-9]+(\.[0-9]+)?([eE][\+\-]?[0-9]+)? { \num -> T.NumLit (read num) }
  [a-zA-Z][a-zA-Z0-9]*                  { \id  -> T.Identifier id     }
  \" [^\"]* \"                          { \str -> T.mkStringLit str   }
  \' [^\']* \'                          { \str -> T.mkStringLit str   }

