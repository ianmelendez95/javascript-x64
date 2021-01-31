module JS.FSModule where 

import Data.Map (Map)
import qualified Data.Map as M
import Control.Monad.State
import Control.Exception
import Control.Monad.Except
import Data.Typeable

import qualified JS.Value as V
import JS.RunState

fsModule :: V.Value
fsModule = V.Object . M.fromList $ [ ("readFileSync", V.ReadFileSync ) ]

readFileSync :: [V.Value] -> RunState V.Value 
readFileSync [V.Str fileName, V.Str "utf8"] = 
  do result <- liftIO fileContents
     case result of 
       Nothing -> lift $ throwError (ENOENT $ "no such file or directory, open '" ++ fileName ++ "'")
       Just content -> return $ V.Str content
  where 
    fileContents :: IO (Maybe String)
    fileContents = (Just <$> readFile fileName) `catch` catchExc
    
    catchExc :: SomeException -> IO (Maybe String)
    catchExc (SomeException e) = do print $ typeOf e
                                    return Nothing