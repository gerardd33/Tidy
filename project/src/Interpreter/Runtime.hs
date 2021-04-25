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
    env <- ask
    return (Nothing, env)

