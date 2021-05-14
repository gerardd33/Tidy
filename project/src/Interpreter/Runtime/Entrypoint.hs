module Interpreter.Runtime.Entrypoint where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.Map                              as Map
import           Data.Maybe
import           Data.Tuple

import           Interpreter.Common.Types
import           Parser.Tidy.Abs

import           Interpreter.Common.Debug
import           Interpreter.Common.Errors
import           Interpreter.Common.Utils.Classes
import           Interpreter.Common.Utils.Environments
import           Interpreter.Common.Utils.Objects
import           Interpreter.Runtime.Environments
import           Interpreter.Runtime.Expressions
import           Interpreter.Runtime.Objects


-- TODO handle debugging in a better way
runtime :: Mode -> ClassEnv -> ClassDecl -> IO (Either RuntimeException Object)
runtime mode classEnv mainClass = runExceptT $ evalStateT
    (runReaderT (runtimeBody mode mainClass) (initialEnvironment classEnv)) initialState

runtimeBody :: Mode -> ClassDecl -> StateMonad Object
runtimeBody mode mainClass = do
    liftIO $ debugLog mode "Runtime..."
    (_, classEnv) <- ask
    (_, initialEnvWithLocal) <- buildInitialLocalReference classEnv
    let mainClassInstanceIdent = singletonInstanceIdentifier $ getClassIdentifier mainClass
    mainClassInstance <- local (const initialEnvWithLocal) $ getLocalObject mainClassInstanceIdent
    (_, initialEnvWithThis) <- local (const initialEnvWithLocal) $ setThisReference mainClassInstance
    -- TODO should call evaluateMemberAction, currently discards args, take logic from ctor call
    result <- local (const initialEnvWithThis) $ evaluateActionInEnv $ fromJust $ getMainAction mainClass
    state <- get
    liftIO $ debugPrint mode "Final state" state
    return $ fst result

buildInitialLocalReference :: ClassEnv -> StateMonad Result
buildInitialLocalReference classEnv = do
    let singletonClasses = Map.toList $ Map.filter ((==MSingleton) . getClassType) classEnv
    let (singletonsIdents, singletonsDeclarations) = unzip singletonClasses
    singletonsAttributes <- mapM evaluateInitializedAttributes singletonsDeclarations
    singletonObjects <- zipWithM buildSingletonClassInstance singletonsIdents singletonsAttributes
    let singletonInstanceIdents = map singletonInstanceIdentifier singletonsIdents
    (_, newEnv) <- addLocalValues $ zip singletonInstanceIdents singletonObjects
    return (pass, newEnv)
