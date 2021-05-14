module Interpreter.Entrypoint.Runtime where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.Map                          as Map
import           Data.Maybe
import           Data.Tuple

import           Interpreter.Common.Types
import           Parser.Tidy.Abs

import           Interpreter.Common.Debug
import           Interpreter.Common.Errors
import           Interpreter.Common.Helper.Classes
import           Interpreter.Common.Helper.Objects
import           Interpreter.Evaluation.Environments
import           Interpreter.Evaluation.Expressions
import           Interpreter.Evaluation.Objects

import qualified Data.Map


-- TODO handle debugging in a better way
runtime :: Mode -> ClassEnv -> ClassDecl -> IO (Either RuntimeException Object)
runtime mode classEnv mainClass = runExceptT $ evalStateT
    (runReaderT (runtimeBody mode mainClass) (initialEnvironment classEnv)) initialState

runtimeBody :: Mode -> ClassDecl -> StateMonad Object
runtimeBody mode mainClass = do
    liftIO $ debugLog mode "Runtime..."
    (_, _, classEnv) <- ask
    (_, initialEnvWithLocal) <- buildInitialLocalObject classEnv
    let mainClassInstanceIdent = singletonInstanceIdentifier $ getClassIdentifier mainClass
    mainClassInstance <- local (const initialEnvWithLocal) $ getLocalAttribute mainClassInstanceIdent
    (_, initialEnvWithThis) <- local (const initialEnvWithLocal) $ setThisReference mainClassInstance
    -- TODO should call evaluateMemberAction, currently discards args, take logic from ctor call
    result <- local (const initialEnvWithThis) $ evaluateActionInEnv $ fromJust $ getMainAction mainClass
    state <- get
    liftIO $ debugPrint mode "Final state" state
    return $ fst result

buildInitialLocalObject :: ClassEnv -> StateMonad Result
buildInitialLocalObject classEnv = do
    let singletonClasses = Map.toList $ Map.filter ((==MSingleton) . getClassType) classEnv
    let (singletonsIdents, singletonsDeclarations) = unzip singletonClasses
    singletonsAttributes <- mapM evaluateInitializedAttributes singletonsDeclarations
    singletonObjects <- zipWithM buildSingletonClassInstance singletonsIdents singletonsAttributes
    let singletonInstanceIdents = map singletonInstanceIdentifier singletonsIdents
    (_, newEnv) <- addLocalValues $ zip singletonInstanceIdents singletonObjects
    return (pass, newEnv)
