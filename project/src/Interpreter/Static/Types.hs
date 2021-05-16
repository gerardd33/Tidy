module Interpreter.Static.Types where

import           Control.Monad.Except
import           Control.Monad.Reader

import           Interpreter.Common.Types
import           Parser.Tidy.Abs

import           Interpreter.Common.Errors
import           Interpreter.Common.Utils.Builtin
import           Interpreter.Common.Utils.Expressions


assertTypesMatch :: String -> ObjectType -> ObjectType -> StaticCheckMonad ObjectType
assertTypesMatch context type1 type2 = do
    when (type1 /= type2) $ throwError $ UnexpectedTypeError (showContext type1) (showContext type2) context
    returnVoid

assertReturnTypesMatch :: String -> ObjectType -> ObjectType -> StaticCheckMonad ObjectType
assertReturnTypesMatch context type1 type2 = do
    when (type1 /= type2) $ throwError $ UnexpectedReturnTypeError (showContext type1) (showContext type2) context
    returnVoid

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

assertPureExpression :: String -> Expr -> StaticCheckMonad ObjectType
assertPureExpression context expr = do
    unless (isExpressionPure expr) $ throwError $ IllegalSideEffectsError context (showContext expr)
    returnVoid
