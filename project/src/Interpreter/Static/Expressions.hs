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
import           Interpreter.Common.Utils.Types
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
checkExpression context (ERelationalOperator expr1 operator expr2) =
    liftPureStatic $ checkBinaryOperator context expr1 expr2 (checkRelationalOperator operator)
checkExpression context (EBooleanOperator expr1 operator expr2) =
    liftPureStatic $ checkBinaryOperator context expr1 expr2 checkBooleanOperator

checkExpression context (EGetExpression getExpr) =
    liftPureStatic $ checkGetExpression context getExpr
checkExpression context (EConstructorCall (CallConstructor classIdent argList)) =
    liftPureStatic $ checkConstructorCall context classIdent argList
checkExpression context (EFunctionalControlFlow (FIfThenElse predicateExpr thenBranch elseBranch)) =
    liftPureStatic $ checkFunctionalIf context predicateExpr thenBranch elseBranch

checkExpression context (EDoExpression doExpr) = liftPureStatic $ checkDoExpression context doExpr
checkExpression context (EImperativeControlFlow (IIf predicateExpr body optionalElseBranch)) =
    liftPureStatic $ checkImperativeIf context predicateExpr body optionalElseBranch
checkExpression context (EImperativeControlFlow (IWhile predicateExpr body)) =
    liftPureStatic $ checkWhile context predicateExpr body

checkExpression _ (ELocalDeclaration localDecl) = checkLocalObjectDeclaration localDecl


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
    classDecl <- getClassDeclarationStatic classIdent
    argTypes <- checkArgumentList context args
    let paramTypes = getConstructorParamTypes classDecl
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

checkGetExpression context (GetExpressionStatic classIdent methodCall) = do
    singletonObjectType <- checkStaticExpression classIdent (showContext methodCall)
    checkGetExpressionOnObject (showContext classIdent) singletonObjectType methodCall

checkGetExpressionOnObject :: String -> ObjectType -> FunctionCall -> StaticCheckMonad ObjectType
checkGetExpressionOnObject context objectType (CallFunction functionIdent ArgumentListAbsent) =
    checkGetExpressionOnObject context objectType (CallFunction functionIdent (ArgumentListPresent []))
checkGetExpressionOnObject context objectType (CallFunction functionIdent (ArgumentListPresent args)) = do
    argTypes <- checkArgumentList context args
    takeGetter <- hasGetterStatic objectType functionIdent
    if takeGetter then checkGetterCall context objectType functionIdent argTypes
    else checkMemberFunctionCall context objectType functionIdent argTypes

checkFunctionalIf :: String -> Expr -> ThenBranch -> ElseBranch -> StaticCheckMonad ObjectType
checkFunctionalIf context predicateExpr thenBranch elseBranch = do
    checkPredicate context predicateExpr True
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

checkImperativeIf :: String -> Expr -> [Expr] -> OptionalElseBranch -> StaticCheckMonad ObjectType
checkImperativeIf context predicateExpr body optionalElseBranch = do
    checkPredicate context predicateExpr False
    (bodyType, _) <- checkExpressionList context body
    elseType <- case optionalElseBranch of
        IElseAbsent -> return bodyType
        IElsePresent elseBody -> returnPureStatic $ checkExpressionList context elseBody
        IElseIf elsePredicateExpr elseBody elseOptionalElseBranch ->
            checkImperativeIf (showContext elseBody) elsePredicateExpr elseBody elseOptionalElseBranch
    assertTypesMatch context bodyType elseType
    return bodyType

checkWhile :: String -> Expr -> [Expr] -> StaticCheckMonad ObjectType
checkWhile context predicateExpr body = do
    checkExpressionList context body
    return voidType

checkPredicate :: String -> Expr -> Bool -> StaticCheckMonad ObjectType
checkPredicate context predicateExpr checkPure = do
    let predicateContext = showComplexContext predicateExpr context
    if checkPure then assertPureExpression predicateContext predicateExpr
    else do (predicateType, _) <- checkExpression predicateContext predicateExpr
            assertTypesMatch predicateContext boolType predicateType

checkDoExpression :: String -> DoExpr -> StaticCheckMonad ObjectType
checkDoExpression context (DoExpressionInstance objectIdent methodCall) = do
    objectType <- checkLocalObject objectIdent
    checkDoExpressionOnObject (showContext objectIdent) objectType methodCall

checkDoExpression context (DoExpressionChain prefixGetExpression methodCall) = do
    (prefixObjectType, _) <- checkExpression context $ EGetExpression prefixGetExpression
    checkDoExpressionOnObject (showContext prefixGetExpression) prefixObjectType methodCall

checkDoExpression context (DoExpressionStatic classIdent methodCall) = do
    singletonObjectType <- checkStaticExpression classIdent (showContext methodCall)
    checkDoExpressionOnObject (showContext classIdent) singletonObjectType methodCall

checkDoExpressionOnObject :: String -> ObjectType -> ActionCall -> StaticCheckMonad ObjectType
checkDoExpressionOnObject context objectType (CallAction actionIdent ArgumentListAbsent) =
    checkDoExpressionOnObject context objectType (CallAction actionIdent (ArgumentListPresent []))
checkDoExpressionOnObject context objectType (CallAction actionIdent (ArgumentListPresent args)) = do
    argTypes <- checkArgumentList context args
    takeSetter <- hasSetterStatic objectType actionIdent
    if takeSetter then checkSetterCall context objectType actionIdent argTypes
    else checkMemberActionCall context objectType actionIdent argTypes

checkLocalObjectDeclaration :: LocalDecl -> StaticCheckMonad StaticResult
checkLocalObjectDeclaration (LocalValueDeclaration objectDecl) =
    checkObjectDeclaration InitializedRequired False objectDecl
checkLocalObjectDeclaration (LocalVariableDeclaration objectDecl) =
    checkObjectDeclaration InitializedRequired True objectDecl
-- TODO variable/value distinction in static local env, split env into two or store this info in addition to type

checkObjectDeclarations :: InitializationType -> Bool -> [ObjectDecl] -> StaticCheckMonad StaticResult
checkObjectDeclarations _ _ [] = liftPureStatic returnVoid
checkObjectDeclarations initializationType isVariable (decl:decls) = do
    (_, env) <- checkObjectDeclaration initializationType isVariable decl
    local (const env) $ checkObjectDeclarations initializationType isVariable decls

checkObjectDeclaration :: InitializationType -> Bool -> ObjectDecl -> StaticCheckMonad StaticResult
checkObjectDeclaration initializationType isVariable (ObjectDeclaration _ objectDeclProper) = do
    case objectDeclProper of
        ObjectDeclarationProper objectIdent objectType initialization -> do
            case initialization of
                 Uninitialized -> when (initializationType == InitializedRequired)
                    (throwError $ UninitializedError $ showContext objectIdent)
                 Initialized expr -> when (initializationType == UninitializedRequired)
                    (throwError $ IllegalInitializationError $ showContext objectIdent)
            checkObjectType objectType
            declareObjectStatic objectDeclProper objectType initialization isVariable

declareObjectStatic :: ObjectDeclProper -> ObjectType -> Initialization -> Bool -> StaticCheckMonad StaticResult
declareObjectStatic properDecl objectType Uninitialized isVariable =
    if isVariable then addLocalVariableType objectIdent objectType else addLocalValueType objectIdent objectType
    where objectIdent = objectIdentifierFromProperDeclaration properDecl

declareObjectStatic properDecl expectedType (Initialized expr) isVariable = do
    let context = showContext properDecl
    (exprType, newEnv) <- checkExpression context expr
    assertTypesMatch context expectedType exprType
    let objectIdent = objectIdentifierFromProperDeclaration properDecl
    assertNoPreviousDuplicateDeclaration context objectIdent
    if isVariable then local (const newEnv) $ addLocalVariableType objectIdent expectedType
    else local (const newEnv) $ addLocalValueType objectIdent expectedType

checkGetterCall :: String -> ObjectType -> MethodIdent -> [ObjectType] -> StaticCheckMonad ObjectType
checkGetterCall context objectType functionIdent argTypes = do
    unless (null argTypes) $ throwError $ MethodArgumentListInvalidError
        (showContext functionIdent) "" (showContext argTypes)
    let attributeIdent = methodToObjectIdentifier functionIdent
    getAttributeTypeStatic context objectType attributeIdent

checkSetterCall :: String -> ObjectType -> MethodIdent -> [ObjectType] -> StaticCheckMonad ObjectType
checkSetterCall context objectType functionIdent argTypes = do
    let attributeIdent = methodToObjectIdentifier functionIdent
    expectedType <- getAttributeTypeStatic context objectType attributeIdent
    unless (length argTypes == 1) $ throwError $ MethodArgumentListInvalidError
            (showContext functionIdent) (showContext expectedType) (showContext argTypes)
    let actualType = head argTypes
    assertTypesMatch context expectedType actualType

checkMemberFunctionCall :: String -> ObjectType -> MethodIdent -> [ObjectType] -> StaticCheckMonad ObjectType
checkMemberFunctionCall context objectType functionIdent argTypes = do
    let classIdent = classIdentifierFromObjectType objectType
    classDecl <- getClassDeclarationStatic classIdent
    let functionType = functionTypeFromClassDeclaration classDecl functionIdent
    when (isNothing functionType) $ throwError $ NoSuchFunctionError context (showContext functionIdent)
    let methodContext = context ++ "." ++ showContext functionIdent
    let implicitArgTypes = [stringType | builtinWithImplicitContext $ builtinMethodIdentifier functionIdent]
    checkMethodArguments methodContext (getMethodParamTypes $ fromJust functionType) (argTypes ++ implicitArgTypes)
    return $ getMethodReturnType $ fromJust functionType

checkMemberActionCall :: String -> ObjectType -> MethodIdent -> [ObjectType] -> StaticCheckMonad ObjectType
checkMemberActionCall context objectType actionIdent argTypes = do
    let classIdent = classIdentifierFromObjectType objectType
    classDecl <- getClassDeclarationStatic classIdent
    let actionType = actionTypeFromClassDeclaration classDecl actionIdent
    when (isNothing actionType) $ throwError $ NoSuchActionError context (showContext actionIdent)
    let methodContext = context ++ "#" ++ showContext actionIdent
    let implicitArgTypes = [stringType | builtinWithImplicitContext $ builtinMethodIdentifier actionIdent]
    liftIO $ print actionIdent
    checkMethodArguments methodContext (getMethodParamTypes $ fromJust actionType) (argTypes ++ implicitArgTypes)
    return $ getMethodReturnType $ fromJust actionType

checkMethodArguments :: String -> [ObjectType] -> [ObjectType] -> StaticCheckMonad ObjectType
checkMethodArguments context expected actual = do
    let argumentTypesMatch = all (uncurry typesMatch) $ zip expected actual
    unless (length expected == length actual && argumentTypesMatch) $ throwError $
            MethodArgumentListInvalidError context (showContext expected) (showContext actual)
    returnVoid

checkStaticExpression :: ClassIdent -> String -> StaticCheckMonad ObjectType
checkStaticExpression classIdent callContext = do
    classDecl <- getClassDeclarationStatic classIdent
    unless (isSingletonClass classDecl) $ throwError $ NonSingletonClassError
        $ showContext classIdent ++ " " ++ callContext
    return $ ObjectTypeClass classIdent GenericParameterAbsent
