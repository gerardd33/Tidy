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
import           Interpreter.Eval.Methods

-- TODO remove or change
import           Interpreter.Eval.Expressions.Evaluate


-- TODO handle debugging in a better way
runtime :: Mode -> ClassEnv -> ClassDecl -> IO (Either RuntimeException Object)
runtime mode classEnv mainClass = runExceptT $ evalStateT
    (runReaderT (runtimeBody mode mainClass) (buildInitialEnvironment classEnv)) buildInitialState

runtimeBody :: Mode -> ClassDecl -> StateMonad Object
runtimeBody mode mainClass = do
    liftIO $ debugLog mode "Runtime..."
    (_, classEnv) <- ask
    (_, initialEnv) <- buildInitialLocalEnv classEnv
    result <- local (const initialEnv) $ evaluateAction $ fromJust $ getMainAction mainClass
    return $ fst result
