module Interpreter.Eval.Utils where

import           Control.Monad.Reader

import           Interpreter.Common.Types
import           Parser.Tidy.Abs

import           Interpreter.Common.Helper.Objects


returnPure :: StateMonad Object -> StateMonad Result
returnPure calculation = do
    value <- calculation
    returnObject value

returnObject :: Object -> StateMonad Result
returnObject value = do
    env <- ask
    return (value, env)

returnPass :: StateMonad Result
returnPass = returnObject pass
