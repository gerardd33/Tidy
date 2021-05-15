module Interpreter.Static.Types where

import           Control.Monad.Reader

import           Interpreter.Common.Types
import           Parser.Tidy.Abs

import           Interpreter.Common.Utils.Objects


returnPureStatic :: StaticCheckMonad StaticResult -> StaticCheckMonad StaticObject
returnPureStatic calculation = do
    (value, _) <- calculation
    return value

liftPureStatic :: StaticCheckMonad StaticObject -> StaticCheckMonad StaticResult
liftPureStatic calculation = do
    env <- ask
    value <- calculation
    return (value, env)

returnPassStatic :: StaticCheckMonad StaticResult
returnPassStatic = liftPureStatic $ return passStatic
