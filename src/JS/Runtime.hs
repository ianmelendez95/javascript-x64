module JS.Runtime (lookupVar, updateVar, assign, newEnvironment) where 

import Data.Map (Map)
import qualified Data.Map as M

import Control.Monad.State.Lazy
import Control.Monad.Except ( ExceptT )
import Data.Maybe

import JS.RunState
import qualified JS.Value as V

import JS.Environment (Environment (..))
import qualified JS.Environment as Env

lookupVar :: String -> RunState V.Value
lookupVar name = fromMaybe V.Undefined <$> gets (Env.lookupBinding name)

updateVar :: String -> V.Value -> RunState ()
updateVar name val = modify (Env.updateBinding name val)

newEnvironment :: [(String, V.Value)] -> RunState ()
newEnvironment vals = modify (Env.pushBindingLayer vals)

assign :: String -> V.Value -> RunState V.Value
assign name val = modify (Env.insertBinding name val) >> return val

