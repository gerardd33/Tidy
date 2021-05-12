module Interpreter.Entrypoint.Runtime where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Maybe
import           Data.Tuple

import           Interpreter.Common.Types
import           Parser.Tidy.Abs

import           Interpreter.Common.Debug
import           Interpreter.Common.Errors
import           Interpreter.Common.Helper.Classes
import           Interpreter.Entrypoint.Initialization
import           Interpreter.Eval.Expressions.Main


-- TODO handle debugging in a better way
runtime :: Mode -> ClassEnv -> ClassDecl -> IO (Either RuntimeException Object)
runtime mode classEnv mainClass = runExceptT $ evalStateT
    (runReaderT (runtimeBody mode mainClass) (initialEnvironment classEnv)) initialState

runtimeBody :: Mode -> ClassDecl -> StateMonad Object
runtimeBody mode mainClass = do
    liftIO $ debugLog mode "Runtime..."
    (_, classEnv) <- ask
    (_, initialEnv) <- buildInitialLocalEnv classEnv
    -- TODO should call evaluateMemberAction
    result <- local (const initialEnv) $ evaluateActionInArgEnv $ fromJust $ getMainAction mainClass
    return $ fst result
