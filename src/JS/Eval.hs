module JS.Eval where 

import Control.Monad.State.Lazy
import Control.Monad.Except
import Control.Monad
import Text.Read hiding (lift)
import Data.Maybe

import JS.RunState
import qualified JS.Syntax as S
import qualified JS.Value as V
import qualified JS.Runtime as R

eval :: S.Expr -> RunState V.Value  
eval (S.Num num)  = pure (V.Num num)
eval (S.Str str)  = pure (V.Str str)
eval (S.Var name) = R.lookupVar name
eval (S.Clt  x y) = V.Bl <$> ((<)  <$> eval x <*> eval y)
eval (S.Cgt  x y) = V.Bl <$> ((>)  <$> eval x <*> eval y)
eval (S.Ceq  x y) = V.Bl <$> ((==) <$> eval x <*> eval y)
eval (S.Clte x y) = V.Bl <$> ((<=) <$> eval x <*> eval y)
eval (S.Cgte x y) = V.Bl <$> ((>=) <$> eval x <*> eval y)
eval (S.Cneq x y) = V.Bl <$> ((/=) <$> eval x <*> eval y)
eval (S.Add  x y) = add <$> eval x <*> eval y
eval (S.Sub  x y) = sub <$> eval x <*> eval y
eval (S.Assign var val) = assign var val
eval (S.For init check update body) = for init check update body

----------------
-- Constructs --
----------------

for :: S.Expr -> S.Expr -> S.Expr -> [S.Expr] -> RunState V.Value
for init check update body = 
  do eval init
     liftIO $ putStrLn "Evaling for"
     cont <- truthy <$> eval check
     if not cont then return V.Undefined
                 else loop >> return V.Undefined 
  where
    loop :: RunState ()
    loop = do mapM_ eval body
              eval update
              cont <- truthy <$> eval check 
              when cont loop

------------------
-- Stateful Ops --
------------------

assign :: S.Expr -> S.Expr -> RunState V.Value
assign (S.Var name) val = 
  do newVal <- eval val
     R.assign name newVal
assign var val = lift $ throwError $ SyntaxError $ "Invalid left-hand side expression in postfix operation: " ++ show (S.Assign var val)

---------------
-- Value Ops --
---------------

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
