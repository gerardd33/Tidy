module Interpreter.Static.Types where

import           Control.Monad.Reader

import           Interpreter.Common.Types
import           Parser.Tidy.Abs

import           Interpreter.Common.Utils.Objects


returnPureStatic :: StaticCheckMonad StaticResult -> StaticCheckMonad ObjectType
returnPureStatic calculation = do
    (returnType, _) <- calculation
    return returnType

liftPureStatic :: StaticCheckMonad ObjectType -> StaticCheckMonad StaticResult
liftPureStatic calculation = do
    env <- ask
    returnType <- calculation
    return (returnType, env)

returnVoid :: StaticCheckMonad ObjectType
returnVoid = return voidType
