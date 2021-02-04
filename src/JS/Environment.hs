module JS.Environment where 

import Data.Map (Map)
import qualified Data.Map as M

import Control.Monad.State.Lazy
import Control.Monad.Except ( ExceptT )
import Data.Maybe

import qualified JS.Value as V

import Debug.Trace

newtype Environment = Environment {
    bindings :: [Map String V.Value]
  }

emptyEnvironment :: Environment
emptyEnvironment = Environment []

initialEnvironment :: [String] -> Environment 
initialEnvironment args = trace ("args: " ++ show args) $
  pushBindingLayer 
    [ ("console", V.Object (M.fromList [("log", V.Function V.ConsoleLog)]))
    , ("require", V.Function V.Require)
    , ("process", V.Object (M.fromList 
                  [("argv", V.arrayFromListWith  V.Str ("hjs" : args))]))
    ] emptyEnvironment

pushBindingLayer :: [(String, V.Value)] -> Environment -> Environment
pushBindingLayer bs env = env { bindings = M.fromList bs : bindings env }

newBindingLayer :: Environment -> Environment
newBindingLayer env = env { bindings = M.empty : bindings env }

lookupBinding :: String -> Environment -> Maybe V.Value
lookupBinding name env = let res = mapMaybe (M.lookup name) (bindings env)
                         in if null res then Nothing else Just (head res)

-- | in lowest environment, insert the binding
insertBinding :: String -> V.Value -> Environment -> Environment
insertBinding name value env = 
  case bindings env of 
    [] -> env { bindings = [M.singleton name value] }
    (b:bs) -> env { bindings = M.insert name value b : tail bs }

-- | update the binding among environments, inserting it into the highest if none found
updateBinding :: String -> V.Value -> Environment -> Environment
updateBinding name val env = Environment { bindings = update (bindings env) }
  where 
    update :: [Map String V.Value] -> [Map String V.Value]
    update [] = [M.singleton name val]
    update [bs] = [M.insert name val bs]
    update (b:bs) = case M.lookup name b of 
                      Nothing -> b : update bs
                      Just _  -> M.insert name val b : bs

