module JS.Runtime where 

import Data.Map (Map)
import qualified Data.Map as M

import Control.Monad.State.Lazy
import Control.Monad.Except ( ExceptT )
import Data.Maybe

import qualified JS.Syntax as S
import qualified JS.Value as V

type RunState = StateT Environment (ExceptT JSError IO)

newtype Environment = Environment {
    bindings :: [Map String V.Value]
  }

newtype JSError = TypeError String deriving (Show)

emptyEnvironment :: Environment 
emptyEnvironment = Environment []

lookupVar :: String -> RunState V.Value
lookupVar name = fromMaybe V.Undefined . lookupBinding name . bindings <$> get

assign :: String -> V.Value -> RunState V.Value
assign name val = modify insert >> return val
  where 
    insert :: Environment -> Environment
    insert env = env { bindings = updateBinding name val $ bindings env }

lookupBinding :: String -> [Map String V.Value] -> Maybe V.Value
lookupBinding name bs = let res = mapMaybe (M.lookup name) bs
                         in if null res then Nothing else Just (head res)

updateBinding :: String -> V.Value -> [Map String V.Value] -> [Map String V.Value]
updateBinding name val [] = [M.singleton name val]
updateBinding name val [bs] = [M.insert name val bs]
updateBinding name val (b:bs) = case M.lookup name b of 
                                  Nothing -> b : updateBinding name val bs
                                  Just _  -> M.insert name val b : bs

