module Interpreter.Static.Methods where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.List                        as List

import           Interpreter.Common.Types
import           Parser.Tidy.Abs

import           Interpreter.Common.Errors
import           Interpreter.Common.Utils.Classes
import           Interpreter.Common.Utils.Methods
import           Interpreter.Common.Utils.Objects
import           Interpreter.Common.Utils.Types
import           Interpreter.Static.Expressions
import           Interpreter.Static.Types


assertNoDeclarationRepetitions :: String -> [MethodIdent] -> StaticCheckMonad ObjectType
assertNoDeclarationRepetitions context idents  = do
    let duplicates = idents List.\\ nub idents
    unless (null duplicates) $ throwError $ DuplicateDeclarationError
        (showContext $ head duplicates) context
    returnVoid

checkFunctionDeclaration :: FunctionDecl -> StaticCheckMonad ObjectType
checkFunctionDeclaration (FunctionDeclaration _ _ functionIdent functionType functionBody) = do
    let paramNames = map objectToMethodIdentifier $ getMethodParamNames functionType
    let withValuesNames = map objectToMethodIdentifier $ getFunctionWithValuesNames functionBody
    assertNoDeclarationRepetitions (showMethodContext functionIdent functionType) $ paramNames ++ withValuesNames
    (_, env) <- checkMethodParams functionIdent functionType
    local (const env) $ checkFunctionBody functionIdent functionType functionBody

checkActionDeclaration :: ActionDecl -> StaticCheckMonad ObjectType
checkActionDeclaration (ActionDeclaration _ _ actionIdent actionType actionBody) = do
    let paramNames = map objectToMethodIdentifier $ getMethodParamNames actionType
    assertNoDeclarationRepetitions (showMethodContext actionIdent actionType) paramNames
    (_, env) <- checkMethodParams actionIdent actionType
    local (const env) $ checkActionBody actionIdent actionType actionBody

checkFunctionBody :: MethodIdent -> MethodType -> FunctionBody -> StaticCheckMonad ObjectType
checkFunctionBody functionIdent functionType (FunctionBodyOneLine bodyExpr) =
    checkFunctionBody functionIdent functionType (FunctionBodyMultiLine bodyExpr WithValuesAbsent)
checkFunctionBody functionIdent functionType (FunctionBodyMultiLine bodyExpr withValues) = do
    let valueDeclarations = case withValues of
            WithValuesPresent values -> valueDeclarationsFromValuesSection values
            _         -> []
    (_, env) <- checkObjectDeclarations InitializedRequired False valueDeclarations
    local (const env) $ checkMethodReturnTypeFromExpression functionIdent functionType bodyExpr
    assertPureExpression (showMethodContext functionIdent functionType) bodyExpr

checkActionBody :: MethodIdent -> MethodType -> ActionBody -> StaticCheckMonad ObjectType
checkActionBody actionIdent actionType (ActionBodyOneLine bodyExpr) =
    checkActionBody actionIdent actionType (ActionBodyMultiLine [bodyExpr])
checkActionBody actionIdent actionType (ActionBodyMultiLine bodyExprs) = do
    (actualReturnType, env) <- checkExpressionList (showMethodContext actionIdent actionType) bodyExprs
    local (const env) $ checkMethodReturnType actionIdent actionType actualReturnType

checkMethodParams :: MethodIdent -> MethodType -> StaticCheckMonad StaticResult
checkMethodParams methodIdent methodType = do
    let paramDeclarations = map publicDeclarationFromProper $ getMethodParamDeclarations methodType
    checkObjectDeclarations UninitializedRequired False paramDeclarations

checkMethodReturnTypeFromExpression :: MethodIdent -> MethodType -> Expr -> StaticCheckMonad ObjectType
checkMethodReturnTypeFromExpression methodIdent methodType bodyExpr = do
    (bodyExprType, _) <- checkExpression (showMethodContext methodIdent methodType) bodyExpr
    checkMethodReturnType methodIdent methodType bodyExprType

checkMethodReturnType :: MethodIdent -> MethodType -> ObjectType -> StaticCheckMonad ObjectType
checkMethodReturnType methodIdent methodType actualReturnType = do
    let expectedReturnType = getMethodReturnType methodType
    assertReturnTypesMatch (showMethodContext methodIdent methodType) expectedReturnType actualReturnType
