module Interpreter.Static.Expressions where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Maybe

import           Interpreter.Common.Types
import           Parser.Tidy.Abs

import           Interpreter.Common.Errors
import           Interpreter.Common.Utils.Builtin
import           Interpreter.Common.Utils.Classes
import           Interpreter.Common.Utils.Expressions
import           Interpreter.Common.Utils.Methods
import           Interpreter.Common.Utils.Objects
import           Interpreter.Static.Environments
import           Interpreter.Static.Operators
import           Interpreter.Static.Types


checkExpressionList :: String -> [Expr] -> StaticCheckMonad StaticResult
checkExpressionList context [] = throwError $ BodyEmptyError context
checkExpressionList context [expr] = checkExpression context expr
checkExpressionList context (expr:exprs) = do
    (_, env) <- checkExpression context expr
    local (const env) $ checkExpressionList context exprs

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
checkExpression context (EGetExpression getExpr) =
    liftPureStatic $ checkGetExpression context getExpr
checkExpression context (EConstructorCall (CallConstructor classIdent argList)) =
    liftPureStatic $ checkConstructorCall context classIdent argList
checkExpression context (EFunctionalControlFlow (FIfThenElse predicateExpr thenBranch elseBranch)) =
    liftPureStatic $ checkFunctionalIf context predicateExpr thenBranch elseBranch

checkExpression context (EImperativeControlFlow (IIf predicateExpr body optionalElseBranch)) =
    checkImperativeIf context predicateExpr body optionalElseBranch
checkExpression context (EImperativeControlFlow (IWhile predicateExpr body)) =
    checkWhile context predicateExpr body
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

checkConstructorCall :: String -> ClassIdent -> ArgList -> StaticCheckMonad ObjectType
checkConstructorCall context classIdent ArgumentListAbsent =
    checkConstructorCall context classIdent (ArgumentListPresent [])
checkConstructorCall context classIdent (ArgumentListPresent args) = do
    classDecl <- getClassDeclStatic classIdent
    when (isNothing classDecl) $ throwError $ ClassNotInScopeError $ showContext classIdent
    argTypes <- checkArgumentList context args
    let paramTypes = getConstructorParamTypes $ fromJust classDecl
    when (argTypes /= paramTypes) $ throwError $
        ConstructorArgumentListInvalidError (showContext classIdent ++ "(" ++ showContext args ++ ")")
            (showContext paramTypes) (showContext argTypes)
    return $ ObjectTypeClass classIdent GenericParameterAbsent

checkArgumentList :: String -> [MethodArg] -> StaticCheckMonad [ObjectType]
checkArgumentList context argList = do
    let argExpressions = argsToExpressionList $ ArgumentListPresent argList
    mapM_ (assertPureExpression context) argExpressions
    checkResults <- mapM (checkExpression context) argExpressions
    return $ map fst checkResults

checkGetExpression :: String -> GetExpr -> StaticCheckMonad ObjectType
checkGetExpression context (GetExpressionInstance objectIdent methodCall) = do
    objectType <- checkLocalObject objectIdent
    checkGetExpressionOnObject (showContext objectIdent) objectType methodCall

checkGetExpression context (GetExpressionChain prefixGetExpression methodCall) = do
    (prefixObjectType, _) <- checkExpression context $ EGetExpression prefixGetExpression
    checkGetExpressionOnObject (showContext prefixGetExpression) prefixObjectType methodCall

-- checkGetExpression :: String -> GetExpr -> StaticCheckMonad ObjectType
-- checkGetExpression context (GetExpressionStatic singletonClass methodCall) = do
--     classDecl <- getClassDeclStatic classIdent
--     when (isNothing classDecl) $ throwError $ ClassNotInScopeError $ showContext classIdent
--     singletonObjectType <- checkExpression $ EGetExpression prefixGetExpression
--     checkGetExpressionOnObject context prefixObjectType methodCall

checkGetExpressionOnObject :: String -> ObjectType -> FunctionCall -> StaticCheckMonad ObjectType
checkGetExpressionOnObject context objectType (CallFunction functionIdent ArgumentListAbsent) =
    checkGetExpressionOnObject context objectType (CallFunction functionIdent (ArgumentListPresent []))
checkGetExpressionOnObject context objectType (CallFunction functionIdent (ArgumentListPresent args)) = do
    argTypes <- checkArgumentList context args
    takeGetter <- hasGetterStatic objectType functionIdent
    if takeGetter && null argTypes
    then checkGetter context objectType functionIdent
    else checkMemberFunction context objectType functionIdent argTypes

checkFunctionalIf :: String -> Expr -> ThenBranch -> ElseBranch -> StaticCheckMonad ObjectType
checkFunctionalIf context predicateExpr thenBranch elseBranch = do
    checkPurePredicate context predicateExpr
    let thenExpr = getThenBranchExpression thenBranch
    let thenContext = showComplexContext thenExpr context
    assertPureExpression thenContext thenExpr
    (thenType, _) <- checkExpression thenContext thenExpr
    (elseType, _) <- case elseBranch of
        FElseOneLine elseExpr -> checkExpression (showComplexContext elseExpr context) elseExpr
        FElseMultiLine elseExpr -> checkExpression (showComplexContext elseExpr context) elseExpr
        FElseIf elsePredicateExpr elseThenBranch elseElseBranch -> liftPureStatic $
            checkFunctionalIf (showContext elseBranch) elsePredicateExpr elseThenBranch elseElseBranch
    assertTypesMatch context thenType elseType
    return thenType

checkImperativeIf :: String -> Expr -> [Expr] -> OptionalElseBranch -> StaticCheckMonad StaticResult
checkImperativeIf context predicateExpr body optionalElseBranch = do
    checkPurePredicate context predicateExpr
    (bodyType, _) <- checkExpressionList context body
    (elseType, _) <- case optionalElseBranch of
        IElseAbsent -> liftPureStatic $ return bodyType
        IElsePresent elseBody -> checkExpressionList context elseBody
        IElseIf elsePredicateExpr elseBody elseOptionalElseBranch ->
            checkImperativeIf (showContext elseBody) elsePredicateExpr elseBody elseOptionalElseBranch
    assertTypesMatch context bodyType elseType
    liftPureStatic $ return bodyType

checkWhile :: String -> Expr -> [Expr] -> StaticCheckMonad StaticResult
checkWhile context predicateExpr body = do
    checkPurePredicate context predicateExpr
    checkExpressionList context body
    liftPureStatic $ return voidType

checkPurePredicate :: String -> Expr -> StaticCheckMonad ObjectType
checkPurePredicate context predicateExpr = do
    let predicateContext = showComplexContext predicateExpr context
    assertPureExpression predicateContext predicateExpr
    (predicateType, _) <- checkExpression predicateContext predicateExpr
    assertTypesMatch predicateContext boolType predicateType


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
            checkObjectType objectType
            declareObjectStatic objectDeclProper objectType initialization

declareObjectStatic :: ObjectDeclProper -> ObjectType -> Initialization -> StaticCheckMonad StaticResult
declareObjectStatic properDecl objectType Uninitialized =
    addLocalObjectType (objectIdentifierFromProperDeclaration properDecl) objectType
declareObjectStatic properDecl expectedType (Initialized expr) = do
    let context = showContext properDecl
    assertPureExpression context expr
    (exprType, newEnv) <- checkExpression context expr
    assertTypesMatch context expectedType exprType
    local (const newEnv) $ addLocalObjectType (objectIdentifierFromProperDeclaration properDecl) expectedType

checkGetter :: String -> ObjectType -> MethodIdent -> StaticCheckMonad ObjectType
checkGetter context objectType functionIdent = do
    let attributeIdent = methodToObjectIdentifier functionIdent
    getAttributeTypeStatic context objectType attributeIdent

checkSetter :: String -> ObjectType -> MethodIdent -> ObjectType -> StaticCheckMonad ObjectType
checkSetter context objectType functionIdent expectedType = do
    let attributeIdent = methodToObjectIdentifier functionIdent
    actualType <- getAttributeTypeStatic context objectType attributeIdent
    assertTypesMatch context expectedType actualType

checkMemberFunction :: String -> ObjectType -> MethodIdent -> [ObjectType] -> StaticCheckMonad ObjectType
checkMemberFunction context objectType functionIdent argTypes = do
    let classIdent = classFromObjectType objectType
    classDecl <- getClassDeclStatic classIdent
    when (isNothing classDecl) $ throwError $ ClassNotInScopeError $ showContext classIdent
    let functionType = functionTypeFromClassDeclaration (fromJust classDecl) functionIdent
    when (isNothing functionType) $ throwError $ NoSuchFunctionError context (showContext functionIdent)
    let paramTypes = getMethodParamTypes $ fromJust functionType
    when (argTypes /= paramTypes) $ throwError $
        MethodArgumentListInvalidError context (showContext paramTypes) (showContext argTypes)
    return $ getMethodReturnType $ fromJust functionType
