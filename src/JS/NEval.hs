module JS.NEval where 

import Control.Monad.Except
import Text.Read hiding (lift)
import Data.Maybe
import qualified Data.Map as M

import JS.RunState
    ( JSError(AccessError, UndefinedVarError), RunState )
import qualified JS.Value as V
import qualified JS.Runtime as R
import qualified JS.Exp as E

evalExprs :: [E.Exp] -> RunState V.Value
evalExprs [] = return V.Undefined
evalExprs [e] = eval e
evalExprs (e:es) = eval e >> evalExprs es

eval :: E.Exp -> RunState V.Value
eval (E.StringLit str) = return $ V.Str str
eval (E.Var var) = R.lookupVar var
eval (E.VarAccess var1 var2) = varAccess var1 var2
eval (E.Assign lhs rhs) = assign lhs rhs
eval _ = error "depr"
-- eval (E.Call fVar args) = do f <- eval fVar
--                              as <- mapM eval args
--                              call f as

---------------
-- Value Ops --
---------------

-- TODO flesh out assign
assign :: E.Exp -> E.Exp -> RunState V.Value
assign (E.Var name) rhs = do value <- eval rhs
                             R.updateVar name value
                             return value
assign _ _ = error "idk"

---- call

-- call :: V.Value -> [V.Value] -> RunState V.Value
-- -- primitives
-- call V.ConsoleLog args = do let output = unwords $ map show args
--                             liftIO $ putStrLn output
--                             return V.Undefined
-- call V.Require args = case args of 
--                         [V.Str "fs"] -> return fsModule
-- call V.ReadFileSync args = readFileSync args

-- compound
-- call (V.Function name params body) args = do R.newEnvironment (zip params (args ++ repeat V.Undefined)) 
--                                              eval body
-- call val args = throwError $ TypeError ("not a function: " ++ show val)

---- value access

varAccess :: String -> String -> RunState V.Value
varAccess var1 var2 = do v1 <- R.lookupVar var1
                         case v1 of 
                           V.Undefined -> throwError $ UndefinedVarError var1
                           (V.Object obj) -> return $ fromMaybe V.Undefined $ M.lookup var2 obj
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
sub _ _ = V.NaN

truthy :: V.Value -> Bool
truthy V.NaN         = False
truthy V.Undefined   = False
truthy V.Null        = False
truthy (V.Num x)     = x /= 0
truthy (V.Bl b)      = b
truthy (V.Str str)   = not $ null str
truthy _ = True

convertToNum :: V.Value -> Maybe Double 
convertToNum (V.Str str) = readMaybe str
convertToNum (V.Num num) = Just num
convertToNum _ = Nothing