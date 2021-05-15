module Interpreter.Static.Types where

import           Control.Monad.Reader

import           Interpreter.Common.Types
import           Parser.Tidy.Abs


returnChecked :: StaticCheckMonad StaticCheckEnv
returnChecked = ask
