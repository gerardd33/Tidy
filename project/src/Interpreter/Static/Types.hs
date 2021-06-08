module Interpreter.Static.Types where

import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.List                            as List

import           Interpreter.Common.Types
import           Parser.Tidy.Abs

import           Interpreter.Common.Errors
import           Interpreter.Common.Utils.Builtin
import           Interpreter.Common.Utils.Expressions


typesMatch :: ObjectType -> ObjectType -> Bool
typesMatch expected actual = expected == anyType || expected == actual

assertTypesMatch :: String -> ObjectType -> ObjectType -> StaticCheckMonad ObjectType
assertTypesMatch context expected actual = do
    unless (typesMatch expected actual) $
        throwError $ UnexpectedTypeError (showContext expected) (showContext actual) context
    returnVoid

assertReturnTypesMatch :: String -> ObjectType -> ObjectType -> StaticCheckMonad ObjectType
assertReturnTypesMatch context expected actual = do
    when (expected /= anyType && expected /= actual) $
        throwError $ UnexpectedReturnTypeError (showContext expected) (showContext actual) context
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

checkTypeUniformity :: String -> [ObjectType] -> StaticCheckMonad ObjectType
checkTypeUniformity context types = do
    if length (List.nub types) /= 1 then throwError $ TypesDoNotMatchError context (showContext types)
    else returnVoid

assertNoDeclarationRepetitions :: String -> [ObjectIdent] -> StaticCheckMonad ObjectType
assertNoDeclarationRepetitions context idents  = do
    let duplicates = idents List.\\ List.nub idents
    unless (null duplicates) $ throwError $ DuplicateDeclarationError
        (showContext $ head duplicates) context
    returnVoid
