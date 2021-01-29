{-# LANGUAGE DeriveGeneric #-}

module JS.Value where

import Data.Map (Map)
import qualified Data.Map
import Data.Vector ( Vector )
import qualified Data.Vector as V
import Data.List

import GHC.Generics
import qualified Data.Aeson as A

import JS.Syntax (Expr)

data Value = Num Double 
           | NaN
           | Bl Bool
           | Str String
           | Object (Map String Value)
           | Array (Vector Value)
           | Undefined
           | Null
           deriving (Generic)

instance A.FromJSON Value
instance A.ToJSON Value where 
  toJSON (Num x)      = A.toJSON x
  toJSON NaN          = A.Null 
  toJSON (Bl b)       = A.toJSON b
  toJSON (Str s)      = A.toJSON s
  toJSON (Object map) = A.toJSON map
  toJSON (Array vect) = A.toJSON vect
  toJSON Undefined    = A.Null
  toJSON Null         = A.Null

instance Show Value where 
  show = show . A.encode

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
