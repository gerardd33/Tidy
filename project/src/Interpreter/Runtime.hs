module Interpreter.Runtime (runtime) where

import           Commons
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.Map                as Map
import           Interpreter.Environment
import           System.IO


-- TODO for now a POC
-- TODO there should be something more to execute passed besides ClassEnv
runtime :: Mode -> ClassEnv -> IO (Either RuntimeException Result)
runtime mode classEnv = runExceptT $ evalStateT (runReaderT (runtimeBody mode classEnv) Map.empty) (Map.empty, 0)

runtimeBody :: Mode -> ClassEnv -> StateMonad Result
runtimeBody mode classEnv = do
    liftIO $ ifDebug mode $ putStrLn "Runtime..."
    -- TODO change environment updating
    (_, env) <- declareValue "testVar" 2137
    (_, env) <- declareValue "anotherVar" 776
    (_, env) <- declareValue "someVar" 3333
    state <- get
    ifDebug mode $ liftIO $ print state
    return (Nothing, env)

-- TODO POC, very simple
declareValue :: Identifier -> Integer -> StateMonad Result
declareValue identifier valueExpr = do
    addValue identifier (IntValue valueExpr)
