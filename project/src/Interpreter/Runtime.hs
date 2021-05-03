module Interpreter.Runtime where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Maybe
import           Data.Tuple

import           Interpreter.Common.Errors
import           Interpreter.Common.Types
import           Interpreter.Common.Utils
import           Interpreter.Eval.Actions
import           Interpreter.Eval.Classes
import           Interpreter.Eval.Environment
import           Parser.Tidy.Abs


runtime :: Mode -> ClassEnv -> ClassDecl -> IO (Either RuntimeException Value)
runtime mode classEnv mainClass = runExceptT $ evalStateT
    (runReaderT (runtimeBody mode classEnv mainClass) (buildInitialEnv classEnv)) buildInitialState

runtimeBody :: Mode -> ClassEnv -> ClassDecl -> StateMonad Value
runtimeBody mode classEnv mainClass = do
    liftIO $ debugLog mode "Runtime..."
    -- TODO initial environment and state updates
    result <- evalAction $ fromJust $ getMainAction mainClass
    return $ fst result
