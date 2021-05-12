module Interpreter.Eval.Utils where

import           Control.Monad.Reader

import           Interpreter.Common.Types
import           Parser.Tidy.Abs

import           Interpreter.Common.Helper.Types


returnPure :: StateMonad Result -> StateMonad Object
returnPure calculation = do
    (value, _) <- calculation
    return value

liftPure :: StateMonad Object -> StateMonad Result
liftPure calculation = do
    value <- calculation
    returnObject value

returnObject :: Object -> StateMonad Result
returnObject value = do
    env <- ask
    return (value, env)

returnPass :: StateMonad Result
returnPass = returnObject pass
