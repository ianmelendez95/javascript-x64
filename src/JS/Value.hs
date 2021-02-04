module JS.Value where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Vector ( Vector, (!?) )
import qualified Data.Vector as V
import Data.List
import Data.Maybe (fromMaybe)

import GHC.Generics
import qualified Data.Aeson as A

import qualified Acorn.Syntax as S

-- TODO seperate into primitives, constructs, etc.
data Value = Num Double 
           | NaN
           | Bl Bool
           | Str String
           | Object Object
           | Array (Vector Value)
           | Function Function
           | Undefined
           | Null

data Function = Func String [String] [S.ExpressionStatement]
              | ConsoleLog 
              | Require
              | ReadFileSync

type Object = Map String Value

instance Show Value where 
  show (Num x)             = show x
  show NaN                 = "NaN"
  show (Bl b)              = show b
  show (Str s)             = s
  show (Object map)        = "[object Object]"
  show (Array vect)        = show vect
  show Undefined           = "undefined"
  show Null                = "null"
  show (Function func)     = show func

instance Show Function where 
  show (Func name _ _)     = "[Function: " ++ name ++ "]"
  show ConsoleLog          = "[Function: log]"
  show Require             = "[Function: require]"
  show ReadFileSync        = "[Function: readFileSync]"

-- | We elect to mimic semantic equivalence, which in JavaScript 
-- | means "primitives are equal by natural equivalence, constructs (arrays, objects, etc.) are 
-- | only equivalent if they point to the same object in memory"
-- | So, until IORefs are implemented, we know no two constructs point to the same object, so they 
-- | are *never* equal (even if they would be in node etc.)
instance Eq Value where 
  (Num x)      == (Num y)      = x == y
  (Bl b)       == (Bl b')      = b == b'
  (Str str)    == (Str str')   = str == str'
  Undefined    == Undefined    = True
  Null         == Null         = True
  _            == _            = False

-- TODO Overhaul this to match node behavior
instance Ord Value where 
  compare (Num x)    (Num y)     = compare x y
  compare (Bl b)     (Bl b')     = compare b b'
  compare (Str str)  (Str str')  = compare str str'
  compare (Object _) (Object _)  = error "Cannot 'compare' objects"
  compare (Array _)  (Array _)   = error "Cannot 'compare' arrays"
  compare Undefined  Undefined   = EQ
  compare Undefined  Null        = EQ
  compare Null       Undefined   = EQ
  compare _          _           = EQ   -- DANGEROUS, here there be bugs

  x <= y = x == y || x < y              -- hopefully these account for that ^
  x >= y = x == y || x > y

memberAccess :: Value -> String -> Value
memberAccess (Object map) key = fromMaybe Undefined $ M.lookup key map
memberAccess _ _ = Undefined

arrayIndex :: Value -> Int -> Value 
arrayIndex (Array vect) index = fromMaybe Undefined (vect !? index) 
arrayIndex _ _ = Undefined

arrayFromListWith :: (a -> Value) -> [a] -> Value
arrayFromListWith f xs = arrayFromList $ map f xs

arrayFromList :: [Value] -> Value
arrayFromList values = Array (V.fromList values)