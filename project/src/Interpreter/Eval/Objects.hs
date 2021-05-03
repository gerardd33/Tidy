module Interpreter.Eval.Objects where

import           Control.Monad.Reader

import           Interpreter.Common.Types
import           Parser.Tidy.Abs


newSingleValueObject :: SingleValue -> Value
newSingleValueObject = SingleValueObject

newRegularObject :: ValueType -> ObjectEnv -> StateMonad Value
newRegularObject objectType objectEnv = return $ RegularObject objectType objectEnv

pass :: Value
pass = newSingleValueObject VoidValue
