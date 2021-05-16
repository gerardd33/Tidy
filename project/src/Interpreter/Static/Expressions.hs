module Interpreter.Static.Expressions where

import           Control.Monad.Except
import           Control.Monad.Reader

import           Interpreter.Common.Types
import           Parser.Tidy.Abs

import           Interpreter.Common.Errors
import           Interpreter.Common.Utils.Builtin
import           Interpreter.Common.Utils.Objects
import           Interpreter.Static.Environments
import           Interpreter.Static.Operators
import           Interpreter.Static.Types


checkExpression :: String -> Expr -> StaticCheckMonad StaticResult
checkExpression _ (ELiteral literal)       = liftPureStatic $ checkLiteral literal
checkExpression _ (ELocalValue identifier) = liftPureStatic $ checkLocalObject identifier
checkExpression context (EAdd expr1 expr2) =
    liftPureStatic $ checkBinaryOperator context expr1 expr2 checkIntegerOperator
checkExpression context (ESubtract expr1 expr2) =
    liftPureStatic $ checkBinaryOperator context expr1 expr2 checkIntegerOperator
checkExpression context (EMultiply expr1 expr2) =
    liftPureStatic $ checkBinaryOperator context expr1 expr2 checkIntegerOperator
checkExpression context (EDivide expr1 expr2) =
    liftPureStatic $ checkBinaryOperator context expr1 expr2 checkIntegerOperator
checkExpression context (EModulo expr1 expr2) =
    liftPureStatic $ checkBinaryOperator context expr1 expr2 checkIntegerOperator
checkExpression context (EConcatenate expr1 expr2) =
    liftPureStatic $ checkBinaryOperator context expr1 expr2 checkConcatenation
checkExpression context (EUnaryNot expr) =
    liftPureStatic $ checkUnaryOperator context expr boolType
checkExpression context (EUnaryMinus expr) =
    liftPureStatic $ checkUnaryOperator context expr intType
checkExpression context (ERelationalOperator expr1 _ expr2) =
    liftPureStatic $ checkBinaryOperator context expr1 expr2 checkRelationalOperator
checkExpression context (EBooleanOperator expr1 operator expr2) =
    liftPureStatic $ checkBinaryOperator context expr1 expr2 checkBooleanOperator

checkExpression _ (ELocalDeclaration localDecl) = checkLocalValueDeclaration localDecl
checkExpression _ _ = liftPureStatic $ return intType


checkBinaryOperator :: String -> Expr -> Expr ->
    (String -> ObjectType -> ObjectType -> Expr -> Expr -> StaticCheckMonad ObjectType) -> StaticCheckMonad ObjectType
checkBinaryOperator context expr1 expr2 typeChecker = do
    assertPureExpression context expr1
    assertPureExpression context expr2
    (type1, _) <- checkExpression context expr1
    (type2, _) <- checkExpression context expr2
    typeChecker context type1 type2 expr1 expr2

checkUnaryOperator :: String -> Expr -> ObjectType -> StaticCheckMonad ObjectType
checkUnaryOperator context expr expectedType = do
    assertPureExpression context expr
    (actualType, _) <- checkExpression context expr
    assertTypesMatch (showComplexContext expr context) expectedType actualType
    return expectedType

checkValuesSection :: InitializationType -> ValuesSection -> StaticCheckMonad StaticResult
checkValuesSection _ ValuesAbsent = liftPureStatic returnVoid
checkValuesSection initializationType (ValuesPresent declarations) =
    checkObjectDeclarations initializationType declarations

checkVariablesSection :: InitializationType -> VariablesSection -> StaticCheckMonad StaticResult
checkVariablesSection _ VariablesAbsent = liftPureStatic returnVoid
checkVariablesSection initializationType (VariablesPresent declarations) =
    checkObjectDeclarations initializationType declarations

checkLocalValueDeclaration :: LocalDecl -> StaticCheckMonad StaticResult
checkLocalValueDeclaration (LocalValueDeclaration objectDecl) =
    checkObjectDeclaration InitializedRequired objectDecl
checkLocalValueDeclaration (LocalVariableDeclaration objectDecl) =
    checkObjectDeclaration InitializedRequired objectDecl
-- TODO variable/value distinction in static local env, split env into two or store this info in addition to type

checkObjectDeclarations :: InitializationType -> [ObjectDecl] -> StaticCheckMonad StaticResult
checkObjectDeclarations _ [] = liftPureStatic returnVoid
checkObjectDeclarations initializationType (decl:decls) = do
    (_, env) <- checkObjectDeclaration initializationType decl
    local (const env) $ checkObjectDeclarations initializationType decls

checkObjectDeclaration :: InitializationType -> ObjectDecl -> StaticCheckMonad StaticResult
checkObjectDeclaration initializationType (ObjectDeclaration _ objectDeclProper) = do
    case objectDeclProper of
        ObjectDeclarationProper objectIdent objectType initialization -> do
            case initialization of
                 Uninitialized -> when (initializationType == InitializedRequired)
                    (throwError $ UninitializedError $ showContext objectIdent)
                 Initialized expr -> when (initializationType == UninitializedRequired)
                    (throwError $ IllegalInitializationError $ showContext objectIdent)
            declareObjectStatic objectDeclProper objectType initialization

declareObjectStatic :: ObjectDeclProper -> ObjectType -> Initialization -> StaticCheckMonad StaticResult
declareObjectStatic properDecl objectType Uninitialized =
    registerLocalObjectType (objectIdentifierFromProperDeclaration properDecl) objectType
declareObjectStatic properDecl expectedType (Initialized expr) = do
    let context = showContext properDecl
    assertPureExpression context expr
    (exprType, newEnv) <- checkExpression context expr
    assertTypesMatch context expectedType exprType
    local (const newEnv) $ registerLocalObjectType (objectIdentifierFromProperDeclaration properDecl) expectedType
