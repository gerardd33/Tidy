module Interpreter.Entrypoint.Initialization where

import qualified Data.Map                                as Map

import           Interpreter.Common.Types
import           Parser.Tidy.Abs

import           Interpreter.Common.Helper.Types
import           Interpreter.Common.Helper.Classes
import           Interpreter.Eval.Expressions.Evaluation
import           Interpreter.Eval.LocalEnvironment


initialEnvironment :: ClassEnv -> Env
initialEnvironment classEnv = (Map.empty, classEnv)

initialState :: RTState
initialState = (Map.empty, 0)

buildInitialLocalEnv :: ClassEnv -> StateMonad Result
buildInitialLocalEnv classEnv = do
    let singletonClasses = Map.toList $ Map.filter isSingletonClass classEnv
    let (classIdents, classDecls) = unzip singletonClasses
    let objectIdents = map singletonInstanceIdentifier classIdents
    singletonObjects <- mapM buildSingletonClassInstance classDecls
    (_, newEnv) <- executeObjectAdditions $ zip objectIdents singletonObjects
    return (pass, newEnv)
