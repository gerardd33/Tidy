module Interpreter.Static.Environments where

import           Control.Monad.Reader
import qualified Data.Map                              as Map

import           Interpreter.Common.Types
import           Parser.Tidy.Abs

import           Interpreter.Common.Utils.Environments
import           Interpreter.Common.Utils.Objects


addLocalValueStatic :: ObjectIdent -> StaticObject -> StaticCheckMonad StaticResult
addLocalValueStatic attributeIdent newObject = do
    (localRef, classEnv) <- ask
    let newValues = Map.insert attributeIdent newObject (getValuesStatic localRef)
    let newLocalRef = newStaticLocalReference $ StaticObjectEnv newValues (getVariablesStatic localRef)
    return (passStatic, (newLocalRef, classEnv))
