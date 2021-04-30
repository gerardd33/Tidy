module Interpreter.Runtime (runtime) where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.Map                as Map
import           System.IO

import           Commons
import           Interpreter.Environment
import           Parser.Tidy.Abs


-- TODO for now a POC
-- TODO there should be something more to execute passed besides ClassEnv
runtime :: Mode -> ClassEnv -> ClassDecl -> IO (Either RuntimeException Result)
runtime mode classEnv mainClass = runExceptT $ evalStateT (runReaderT (runtimeBody mode classEnv mainClass) Map.empty) (Map.empty, 0)

runtimeBody :: Mode -> ClassEnv -> ClassDecl -> StateMonad Result
runtimeBody mode classEnv mainClass = do
    liftIO $ debugLog mode "Runtime..."
    -- TODO initial environment and state updates
    env <- ask
    return (Nothing, env)

-- TODO POC, very simple
declareValue :: Identifier -> Integer -> StateMonad Result
declareValue identifier valueExpr = do
    addValue identifier (IntValue valueExpr)
