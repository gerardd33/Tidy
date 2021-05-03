module Interpreter.Eval.Objects where

import           Interpreter.Common.Types
import           Interpreter.Eval.Classes
import           Parser.Tidy.Abs


newSingleValueObject :: SingleValue -> Value
newSingleValueObject = SingleValueObject

newRegularObject :: ValueType -> ValueEnv -> StateMonad Value
newRegularObject objectType args = do
    objectEnv <- objectEnvFromArgs objectType args
    return $ RegularObject objectType objectEnv

pass :: Value
pass = newSingleValueObject VoidValue

objectEnvFromArgs :: ValueType -> ValueEnv -> StateMonad ObjectEnv
-- objectEnvFromArgs objectType args = do
--     (localEnv, classEnv) <- ask
--     let objectValueList = getValueList objectType
--     let (values, variables) = partition (`elem` objectValueList) args
objectEnvFromArgs = undefined
