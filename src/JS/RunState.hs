module JS.RunState where

import Control.Monad.State.Lazy
import Control.Monad.Except ( ExceptT )
import Data.Maybe

import qualified JS.Environment as Env

type RunState = StateT Env.Environment (ExceptT JSError IO)

data JSError = SyntaxError String
             | AccessError String
             | UndefinedVarError String
             | TypeError String

instance Show JSError where 
  show (SyntaxError msg) = "SyntaxError: " ++ msg
  show (AccessError msg) = "AccessError: " ++ msg
  show (UndefinedVarError msg) = "UndefinedVarError: " ++ msg
  show (TypeError msg) = "TypeError: " ++ msg