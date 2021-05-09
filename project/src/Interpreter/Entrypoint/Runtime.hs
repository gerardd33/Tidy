module Interpreter.Entrypoint.Runtime where

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
import           Interpreter.Eval.Expressions
import           Parser.Tidy.Abs


runtime :: Mode -> ClassEnv -> ClassDecl -> IO (Either RuntimeException Value)
runtime mode classEnv mainClass = runExceptT $ evalStateT
    (runReaderT (runtimeBody mode mainClass) (buildInitialEnv classEnv)) buildInitialState

runtimeBody :: Mode -> ClassDecl -> StateMonad Value
runtimeBody mode mainClass = do
    liftIO $ debugLog mode "Runtime..."
    (_, classEnv) <- ask
    (_, initialEnv) <- buildInitialLocalEnv classEnv
    result <- local (const initialEnv) $ evalAction $ fromJust $ getMainAction mainClass
    return $ fst result
