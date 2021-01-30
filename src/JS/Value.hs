{-# LANGUAGE DeriveGeneric #-}

module JS.Value where

import Data.Map (Map)
import qualified Data.Map
import Data.Vector ( Vector )
import qualified Data.Vector as V
import Data.List

import GHC.Generics
import qualified Data.Aeson as A

import JS.Exp

data Value = Num Double 
           | NaN
           | Bl Bool
           | Str String
           | Object (Map String Value)
           | Array (Vector Value)
           | Function String [String] Exp
           | Undefined
           | ConsoleLog -- builtin
           | Null

instance Show Value where 
  show (Num x)             = show x
  show NaN                 = "NaN"
  show (Bl b)              = show b
  show (Str s)             = s
  show (Object map)        = "[object Object]"
  show (Array vect)        = show vect
  show Undefined           = "undefined"
  show Null                = "null"
  show (Function name _ _) = "[Function: " ++ name ++ "]"
  show ConsoleLog          = "[Function: log]"

-- | Strict equality
instance Eq Value where 
  (Num x)    == (Num y)     = x == y
  (Bl b)     == (Bl b')     = b == b'
  (Str str)  == (Str str')  = str == str'
  (Object o) == (Object o') = o == o'
  (Array a)  == (Array a')  = a == a'
  Undefined  == Undefined   = True
  Undefined  == Null        = True
  Null       == Undefined   = True
  _          == _           = False

instance Ord Value where 
  compare (Num x)    (Num y)     = compare x y
  compare (Bl b)     (Bl b')     = compare b b'
  compare (Str str)  (Str str')  = compare str str'
  compare (Object o) (Object o') = compare o o'
  compare (Array a)  (Array a')  = compare a a'
  compare Undefined  Undefined   = EQ
  compare Undefined  Null        = EQ
  compare Null       Undefined   = EQ
  compare _          _           = EQ   -- DANGEROUS, here there be bugs

  x <= y = x == y || x < y              -- hopefully these account for that
  x >= y = x == y || x > y
