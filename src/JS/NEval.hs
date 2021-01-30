module JS.NEval where 

import Control.Monad.State.Lazy
import Control.Monad.Except
import Control.Monad
import Text.Read hiding (lift)
import Data.Maybe
import qualified Data.Map as M

import JS.RunState
import qualified JS.Value as V
import qualified JS.Runtime as R
import qualified JS.Exp as E

eval :: E.Exp -> RunState V.Value
eval (E.StringLit str) = return $ V.Str str
eval (E.Var var) = R.lookupVar var
eval (E.VarAccess var1 var2) = varAccess var1 var2

---------------
-- Value Ops --
---------------

varAccess :: String -> String -> RunState V.Value
varAccess var1 var2 = do v1 <- R.lookupVar var1
                         case v1 of 
                           V.Undefined -> throwError $ UndefinedVarError var1
                           (V.Object map) -> return $ fromMaybe V.Undefined $ M.lookup var2 map
                           other -> throwError $ AccessError $ var1 ++ " did not resolve to an object, rather: " ++ show other

applyNums :: (Double -> Double -> a) -> V.Value -> V.Value -> Maybe a
applyNums f x y = do xNum <- convertToNum x
                     yNum <- convertToNum y 
                     return $ f xNum yNum

add :: V.Value -> V.Value -> V.Value
add (V.Num x) (V.Num y) = V.Num (x + y)
add x y = V.Str $ show x ++ show y

sub :: V.Value -> V.Value -> V.Value
sub (V.Num x) (V.Num y) = V.Num (x - y)
sub x y = V.NaN

truthy :: V.Value -> Bool
truthy V.NaN        = False
truthy V.Undefined  = False
truthy V.Null       = False
truthy (V.Num x)    = x /= 0
truthy (V.Bl b)     = b
truthy (V.Str str)  = not $ null str
truthy (V.Object _) = True
truthy (V.Array _)  = True

convertToNum :: V.Value -> Maybe Double 
convertToNum (V.Str str) = readMaybe str
convertToNum (V.Num num) = Just num
convertToNum _ = Nothing