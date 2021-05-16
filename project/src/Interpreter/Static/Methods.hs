module Interpreter.Static.Methods where

import           Control.Monad.Except
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

checkFunctionDeclaration :: FunctionDecl -> StaticCheckMonad ObjectType
checkFunctionDeclaration (FunctionDeclaration _ _ functionIdent functionType functionBody) = do
    (_, env) <- checkMethodParams functionIdent functionType
    returnVoid

checkMethodParams :: MethodIdent -> MethodType -> StaticCheckMonad StaticResult
checkMethodParams methodIdent methodType = do
    let paramNames = map objectToMethodIdentifier $ getMethodParamNames methodType
    assertNoDeclarationRepetitions (showContext methodIdent ++ ": " ++ showContext methodType) paramNames
    let paramDeclarations = map publicDeclarationFromProper $ getMethodParamDeclarations methodType
    checkObjectDeclarations False paramDeclarations
    -- TODO change Bool here to something three-valued: must not initialize

