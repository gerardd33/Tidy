module Interpreter.Static.Environments where

import           Control.Monad.Reader
import qualified Data.Map                              as Map

import           Interpreter.Common.Types
import           Parser.Tidy.Abs

import           Interpreter.Common.Utils.Environments
import           Interpreter.Common.Utils.Objects


registerLocalObjectType :: ObjectIdent -> ObjectType -> StaticCheckMonad StaticResult
registerLocalObjectType objectIdent objectType = do
    (localEnv, classEnv) <- ask
    let newValues = Map.insert objectIdent objectType (valueTypes localEnv)
    let newLocalEnv = StaticLocalEnv newValues (variableTypes localEnv)
    return (voidType, (newLocalEnv, classEnv))
