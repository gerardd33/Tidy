module Interpreter.Runtime (runtime) where

import           Commons

runtime :: ClassEnv -> IO ()
runtime classEnv = do
    putStrLn "Runtime..."
    print classEnv


