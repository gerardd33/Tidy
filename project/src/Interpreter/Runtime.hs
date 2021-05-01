module Interpreter.Runtime (runtime) where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.Map                as Map
import           Data.Maybe
import           System.IO

import           Interpreter.Classes
import           Interpreter.Commons
import           Interpreter.Environment
import           Interpreter.Expressions
import           Interpreter.Functions
import           Parser.Tidy.Abs


-- TODO put mode in the env or somewhere
-- TODO for now a POC
-- TODO there should be something more to execute passed besides ClassEnv
runtime :: Mode -> ClassEnv -> ClassDecl -> IO (Either RuntimeException Result)
runtime mode classEnv mainClass = runExceptT $ evalStateT (runReaderT (runtimeBody mode classEnv mainClass) Map.empty) (Map.empty, 0)

-- TODO later put classEnv in the env or do something with it
runtimeBody :: Mode -> ClassEnv -> ClassDecl -> StateMonad Result
runtimeBody mode classEnv mainClass = do
    liftIO $ debugLog mode "Runtime..."
    -- TODO initial environment and state updates
    evalAction $ fromJust $ getMainAction mainClass

-- TODO passing parameters and other context information
evalAction :: ActionDecl -> StateMonad Result
evalAction action = do
    evalActionBody (getActionBody action)

evalActionBody :: ActionBody -> StateMonad Result
evalActionBody (ActionBodyOneLine expr)    = evalExpressionList [expr]
evalActionBody (ActionBodyMultiLine exprs) = evalExpressionList exprs
