module Interpreter.Static.Methods where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.List                        as List

import           Interpreter.Common.Types
import           Parser.Tidy.Abs

import           Interpreter.Common.Errors
import           Interpreter.Common.Utils.Methods
import           Interpreter.Common.Utils.Objects
import           Interpreter.Static.Expressions
import           Interpreter.Static.Objects
import           Interpreter.Static.Types


assertNoDeclarationRepetitions :: String -> [MethodIdent] -> StaticCheckMonad ObjectType
assertNoDeclarationRepetitions context idents  = do
    let duplicates = idents List.\\ nub idents
    unless (null duplicates) $ throwError $ DuplicateDeclarationError
        (showContext $ head duplicates) context
    returnVoid

checkFunctionsSection :: FunctionsSection -> StaticCheckMonad ObjectType
checkFunctionsSection FunctionsAbsent = returnVoid
checkFunctionsSection (FunctionsPresent declarations) = do
    mapM_ checkFunctionDeclaration declarations >> returnVoid

checkFunctionDeclaration :: FunctionDecl -> StaticCheckMonad ObjectType
checkFunctionDeclaration (FunctionDeclaration _ _ functionIdent functionType functionBody) = do
    (_, env) <- checkMethodParams functionIdent functionType
    local (const env) $ checkFunctionBody functionIdent functionType functionBody
    returnVoid

checkMethodParams :: MethodIdent -> MethodType -> StaticCheckMonad StaticResult
checkMethodParams methodIdent methodType = do
    let paramNames = map objectToMethodIdentifier $ getMethodParamNames methodType
    assertNoDeclarationRepetitions (showMethodContext methodIdent methodType) paramNames
    let paramDeclarations = map publicDeclarationFromProper $ getMethodParamDeclarations methodType
    checkObjectDeclarations UninitializedRequired paramDeclarations

checkFunctionBody :: MethodIdent -> MethodType -> FunctionBody -> StaticCheckMonad ObjectType
checkFunctionBody functionIdent functionType (FunctionBodyOneLine expr) =
    checkFunctionBody functionIdent functionType (FunctionBodyMultiLine expr WithValuesAbsent)
checkFunctionBody functionIdent functionType (FunctionBodyMultiLine expr withValues) = do
    let valuesSection = case withValues of
            WithValuesAbsent         -> ValuesAbsent
            WithValuesPresent values -> values
    (_, env) <- checkValuesSection InitializedRequired valuesSection
    local (const env) $ checkFunctionReturnType functionIdent functionType expr

checkFunctionReturnType :: MethodIdent -> MethodType -> Expr -> StaticCheckMonad ObjectType
checkFunctionReturnType functionIdent functionType bodyExpr = do
    let returnType = getMethodReturnType functionType
    (bodyExprType, _) <- checkExpression bodyExpr
    assertReturnTypesMatch (showMethodContext functionIdent functionType) returnType bodyExprType
