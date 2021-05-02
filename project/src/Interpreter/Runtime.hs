module Interpreter.Runtime (runtime) where

import           Control.Monad.Except
import           Control.Monad.State
import qualified Data.Map                as Map
import           Data.Maybe
import           System.IO

import           Interpreter.Classes
import           Interpreter.Commons
import           Interpreter.Expressions
import           Interpreter.Functions
import           Interpreter.State
import           Parser.Tidy.Abs


runtime :: Mode -> ClassEnv -> ClassDecl -> IO (Either RuntimeException Value)
runtime mode classEnv mainClass = runExceptT $ evalStateT (runtimeBody mode classEnv mainClass) buildInitialState

runtimeBody :: Mode -> ClassEnv -> ClassDecl -> StateMonad Value
runtimeBody mode classEnv mainClass = do
    liftIO $ debugLog mode "Runtime..."
    -- TODO initial environment and state updates
    evalAction $ fromJust $ getMainAction mainClass

-- TODO passing parameters and other context information
evalAction :: ActionDecl -> StateMonad Value
evalAction action = evalActionBody (getActionBody action)

evalActionBody :: ActionBody -> StateMonad Value
evalActionBody (ActionBodyOneLine expr)    = evalExpressionList [expr]
evalActionBody (ActionBodyMultiLine exprs) = evalExpressionList exprs
