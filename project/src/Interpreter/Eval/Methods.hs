module Interpreter.Eval.Methods where

import           Control.Monad.Reader
import qualified Data.Map                          as Map

import           Interpreter.Common.Types
import           Parser.Tidy.Abs

import           Interpreter.Common.Helper.Methods
import           Interpreter.Common.Helper.Objects
import           Interpreter.Eval.Environments


-- TODO handle builtin objects
evaluateGetter :: Object -> MethodIdent -> StateMonad Object
evaluateGetter (RegularObject _ objectEnv) functionIdent = do
    let attribute = methodToObjectIdentifier functionIdent
    if attribute `Map.member` values objectEnv
    then retrieveObject $ values objectEnv Map.! attribute
    else retrieveObject $ variables objectEnv Map.! attribute

evaluateSetter :: Object -> MethodIdent -> Object -> StateMonad Result
evaluateSetter (RegularObject _ objectEnv) actionIdent newValue = do
    let attribute = methodToObjectIdentifier actionIdent
    if attribute `Map.member` values objectEnv
    then setObject (values objectEnv Map.! attribute) newValue
    else setObject (variables objectEnv Map.! attribute) newValue

addArgumentsToEnv :: MethodType -> [Object] -> StateMonad Result
addArgumentsToEnv methodType evaluatedArgs = do
    (localEnv, classEnv) <- ask
    let methodParamList = getMethodParamList methodType
    let decls = zip methodParamList evaluatedArgs
    (_, newEnv) <- addLocalValues decls
    return (pass, newEnv)
