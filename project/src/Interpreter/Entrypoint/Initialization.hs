module Interpreter.Entrypoint.Initialization where

import           Control.Monad
import qualified Data.Map                          as Map

import           Interpreter.Common.Types
import           Parser.Tidy.Abs

import           Interpreter.Common.Helper.Classes
import           Interpreter.Common.Helper.Objects
import           Interpreter.Eval.Expressions.Main
import           Interpreter.Eval.LocalEnvironment
import           Interpreter.Eval.Objects


initialEnvironment :: ClassEnv -> Env
initialEnvironment classEnv = (Map.empty, classEnv)

initialState :: RTState
initialState = (Map.empty, 0)

buildInitialLocalEnv :: ClassEnv -> StateMonad Result
buildInitialLocalEnv classEnv = do
    let singletonClasses = Map.toList $ Map.filter ((==MSingleton) . getClassType) classEnv
    let (singletonsIdents, singletonsDecls) = unzip singletonClasses
    let objectIdents = map singletonInstanceIdentifier singletonsIdents
    singletonsAttributes <- mapM evaluateInitializedAttributes singletonsDecls
    singletonObjects <- zipWithM buildSingletonClassInstance singletonsIdents singletonsAttributes
    (_, newEnv) <- addLocalValues $ zip objectIdents singletonObjects
    return (pass, newEnv)
