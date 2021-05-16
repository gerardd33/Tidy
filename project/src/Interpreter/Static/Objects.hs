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


checkObjectDeclarations :: Bool -> [ObjectDecl] -> StaticCheckMonad StaticResult
checkObjectDeclarations _ [] = liftPureStatic returnVoid
checkObjectDeclarations shouldInitialize (decl:decls) = do
    (_, env) <- checkObjectDeclaration shouldInitialize decl
    local (const env) $ checkObjectDeclarations shouldInitialize decls

checkObjectDeclaration :: Bool -> ObjectDecl -> StaticCheckMonad StaticResult
checkObjectDeclaration shouldInitialize (ObjectDeclaration _ objectDeclProper) = do
    case objectDeclProper of
        ObjectDeclarationProper objectIdent objectType initialization -> case initialization of
             Uninitialized -> when shouldInitialize (throwError $ UninitializedError $ showContext objectIdent) >>
                liftPureStatic returnVoid
             Initialized expr -> declareObjectStatic (showContext objectDeclProper) objectType expr

declareObjectStatic :: String -> ObjectType -> Expr -> StaticCheckMonad StaticResult
declareObjectStatic context expectedType expr = do
    (exprType, newEnv) <- checkExpression expr
    assertTypesMatch context expectedType exprType
    return (voidType, newEnv)
