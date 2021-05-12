module Interpreter.Eval.Methods where

import           Control.Monad.Reader
import qualified Data.Map                          as Map

import           Interpreter.Common.Types
import           Parser.Tidy.Abs

import           Interpreter.Common.Helper.Methods
import           Interpreter.Common.Helper.Objects
import           Interpreter.Eval.LocalEnvironment


-- TODO handle builtin objects
evaluateGetter :: Object -> MethodIdent -> StateMonad Object
evaluateGetter (RegularObject _ objectEnv) functionIdent = do
    let attribute = methodToObjectIdentifier functionIdent
    if attribute `Map.member` values objectEnv
    then return $ values objectEnv Map.! attribute
    else return $ variables objectEnv Map.! attribute

addArgumentsToEnv :: FunctionDecl -> [Object] -> StateMonad Result
addArgumentsToEnv function evaluatedArgs = do
    (localEnv, classEnv) <- ask
    let methodParamList = getMethodParamList $ getFunctionType function
    let decls = zip methodParamList evaluatedArgs
    (_, newEnv) <- addLocalValues decls
    return (pass, newEnv)
