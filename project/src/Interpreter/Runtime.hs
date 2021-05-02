module Interpreter.Runtime (runtime) where

import           Control.Monad.Except
import           Control.Monad.State
import           Data.Maybe

import           Interpreter.Actions
import           Interpreter.Classes
import           Interpreter.Environment
import           Interpreter.Utils
import           Parser.Tidy.Abs


runtime :: Mode -> ClassEnv -> ClassDecl -> IO (Either RuntimeException Value)
runtime mode classEnv mainClass = runExceptT $ evalStateT (runtimeBody mode classEnv mainClass) buildInitialState

runtimeBody :: Mode -> ClassEnv -> ClassDecl -> StateMonad Value
runtimeBody mode classEnv mainClass = do
    liftIO $ debugLog mode "Runtime..."
    -- TODO initial environment and state updates
    evalAction $ fromJust $ getMainAction mainClass
