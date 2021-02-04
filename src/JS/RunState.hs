module JS.RunState where

import Control.Monad.State.Lazy ( StateT )
import Control.Monad.Except ( ExceptT )

import qualified JS.Environment as Env

type RunState = StateT Env.Environment (ExceptT JSError IO)

data JSError = SyntaxError String
             | AccessError String
             | UndefinedVarError String
             | TypeError String
             | ReferenceError String
             | ENOENT String
             | RawError String

instance Show JSError where 
  show (SyntaxError msg) = "SyntaxError: " ++ msg
  show (AccessError msg) = "AccessError: " ++ msg
  show (UndefinedVarError msg) = "UndefinedVarError: " ++ msg
  show (TypeError msg) = "TypeError: " ++ msg
  show (ReferenceError msg) = "ReferenceError: " ++ msg
  show (ENOENT msg) = "ENOENT: " ++ msg
  show (RawError msg) = msg