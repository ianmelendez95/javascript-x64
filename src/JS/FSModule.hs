module JS.FSModule where 

import Data.Map (Map)
import qualified Data.Map as M
import Control.Monad.State

import qualified JS.Value as V
import JS.RunState

fsModule :: V.Value
fsModule = V.Object . M.fromList $ [ ("readFileSync", V.ReadFileSync ) ]

readFileSync :: [V.Value] -> RunState V.Value 
readFileSync [V.Str fileName, V.Str "utf8"] = V.Str <$> liftIO (readFile fileName)