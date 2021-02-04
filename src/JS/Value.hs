module JS.Value where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Vector ( Vector, (!?) )
import qualified Data.Vector as V
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

import qualified Acorn.Syntax as S

-- TODO seperate into primitives, constructs, etc. (maybe idk)
data Value = Num Double 
           | NaN
           | Bl Bool
           | Str String
           | Object Object
           | Array Object (Vector Value)
           | Function Object Function
           | Undefined
           | Null

data Function = Func String [String] [S.ExpressionStatement]
              | ConsoleLog 
              | Require
              | ReadFileSync

type Object = Map String Value

-- TODO write a IsJSValue class, and see if there's some language extension to auto convert

instance Show Value where 
  show (Num x)             = show x
  show NaN                 = "NaN"
  show (Bl b)              = show b
  show (Str s)             = s
  show (Object _)          = "[object Object]"
  show (Array _ vect)      = show vect -- TODO do not include the angle brackets
  show Undefined           = "undefined"
  show Null                = "null"
  show (Function _ func)   = show func

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
  compare _          _           = error "can only compare primitives"

  x <= y = x == y || x < y              -- hopefully these account for that ^
  x >= y = x == y || x > y

consoleLog :: Value 
consoleLog = Function M.empty ConsoleLog

require :: Value 
require = Function M.empty Require

readFileSync :: Value 
readFileSync = Function M.empty ReadFileSync

-- | TODO update values to have object properties
-- | consider how we're going to throw errors (since we can't 
-- | import RunState)
memberAccess :: Value -> String -> Value
memberAccess (Object obj) key = fromMaybe Undefined $ M.lookup key obj
memberAccess (Array obj vect) key 
  = fromMaybe Undefined $ case readMaybe key :: Maybe Int of 
                            Just index -> vect !? index
                            Nothing -> M.lookup key obj
memberAccess _ _ = undefined

objectFromList :: [(String, Value)] -> Value
objectFromList ls = Object (M.fromList ls)

arrayFromListWith :: (a -> Value) -> [a] -> Value
arrayFromListWith f xs = arrayFromList $ map f xs

arrayFromList :: [Value] -> Value
arrayFromList values = Array M.empty (V.fromList values)