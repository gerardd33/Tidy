module Interpreter.Runtime.Types where

import           Control.Monad.Reader
import qualified Data.Map                         as Map

import           Interpreter.Common.Types
import           Parser.Tidy.Abs

import           Interpreter.Common.Utils.Builtin


returnPure :: StateMonad Result -> StateMonad Object
returnPure calculation = do
    (value, _) <- calculation
    return value

liftPure :: StateMonad Object -> StateMonad Result
liftPure calculation = do
    env <- ask
    value <- calculation
    return (value, env)

returnPass :: StateMonad Result
returnPass = liftPure $ return pass
