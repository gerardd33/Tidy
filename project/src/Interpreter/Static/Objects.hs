module Interpreter.Static.Objects where

import           Control.Monad.Reader
import           Control.Monad.Except

import           Interpreter.Common.Types
import           Parser.Tidy.Abs

import           Interpreter.Common.Errors
import           Interpreter.Common.Utils.Builtin
import           Interpreter.Common.Utils.Objects
import           Interpreter.Static.Expressions
import           Interpreter.Static.Types


checkObjectDeclarations :: InitializationType -> [ObjectDecl] -> StaticCheckMonad StaticResult
checkObjectDeclarations _ [] = liftPureStatic returnVoid
checkObjectDeclarations initializationType (decl:decls) = do
    (_, env) <- checkObjectDeclaration initializationType decl
    local (const env) $ checkObjectDeclarations initializationType decls

checkObjectDeclaration :: InitializationType -> ObjectDecl -> StaticCheckMonad StaticResult
checkObjectDeclaration initializationType (ObjectDeclaration _ objectDeclProper) = do
    case objectDeclProper of
        ObjectDeclarationProper objectIdent objectType initialization -> case initialization of
             Uninitialized -> when (initializationType == InitializedRequired)
                (throwError $ UninitializedError $ showContext objectIdent) >> liftPureStatic returnVoid
             Initialized expr -> if initializationType == UninitializedRequired
                then throwError (IllegalInitializationError $ showContext objectIdent) >> liftPureStatic returnVoid
                else declareObjectStatic (showContext objectDeclProper) objectType expr

declareObjectStatic :: String -> ObjectType -> Expr -> StaticCheckMonad StaticResult
declareObjectStatic context expectedType expr = do
    (exprType, newEnv) <- checkExpression expr
    assertTypesMatch context expectedType exprType
    return (voidType, newEnv)
