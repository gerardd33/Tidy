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
import           Interpreter.Common.Helper.Methods
import           Interpreter.Eval.Expressions
import           Interpreter.Eval.Methods


-- TODO handle debug better
runtime :: Mode -> ClassEnv -> ClassDecl -> IO (Either RuntimeException Value)
runtime mode classEnv mainClass = runExceptT $ evalStateT
    (runReaderT (runtimeBody mode mainClass) (buildInitialEnvironment classEnv)) buildInitialState

runtimeBody :: Mode -> ClassDecl -> StateMonad Value
runtimeBody mode mainClass = do
    liftIO $ debugLog mode "Runtime..."
    (_, classEnv) <- ask
    (_, initialEnv) <- buildInitialLocalEnv classEnv
    result <- local (const initialEnv) $ evaluateAction $ fromJust $ getMainAction mainClass
    return $ fst result
