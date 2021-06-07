module Interpreter.Static.Expressions where

import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.Map                             as Map
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
import           Interpreter.Static.Generics
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
checkExpression context (EConstructorCall (CallConstructor classType argList)) =
    liftPureStatic $ checkConstructorCall context classType argList
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

checkConstructorCall :: String -> ClassType -> ArgList -> StaticCheckMonad ObjectType
checkConstructorCall context classType ArgumentListAbsent =
    checkConstructorCall context classType (ArgumentListPresent [])
checkConstructorCall context classType (ArgumentListPresent args) = do
    classDecl <- getClassDeclarationStatic classType
    argTypes <- checkArgumentList context args
    let unmappedParamTypes = getConstructorParamTypes classDecl
    let callContext = show classType ++ "(" ++ showContext args ++ ")"
    let genericParams = getGenericParameterList classDecl
    let genericArgs = genericParameterListFromClassType classType
    genericsMap <- bindGenericParameters callContext genericParams genericArgs
    let paramTypes = map (mapTypeIfGeneric genericsMap) unmappedParamTypes
    when (argTypes /= paramTypes) $ throwError $
        ConstructorArgumentListInvalidError callContext (showContext paramTypes) (showContext argTypes)
    return $ ObjectTypeClass classType

checkArgumentList :: String -> [MethodArg] -> StaticCheckMonad [ObjectType]
checkArgumentList context argList = do
    let argExpressions = argsToExpressionList $ ArgumentListPresent argList
    mapM_ (assertPureExpression context) argExpressions
    checkResults <- mapM (checkExpression context) argExpressions
    return $ map fst checkResults

checkGetExpression :: String -> GetExpr -> StaticCheckMonad ObjectType
checkGetExpression context (GetExpressionInstance objectIdent methodCall) = do
    objectType <- checkLocalObject objectIdent
    genericsMap <- genericsMapFromClassType $ classTypeFromObjectType objectType
    checkGetExpressionOnObject (showContext objectIdent) genericsMap objectType methodCall

checkGetExpression context (GetExpressionChain prefixGetExpression methodCall) = do
    (prefixObjectType, _) <- checkExpression context $ EGetExpression prefixGetExpression
    genericsMap <- genericsMapFromClassType $ classTypeFromObjectType prefixObjectType
    checkGetExpressionOnObject (showContext prefixGetExpression) genericsMap prefixObjectType methodCall

checkGetExpression context (GetExpressionStatic classType methodCall) = do
    (singletonObjectType, genericParams) <- checkStaticExpression classType (showContext methodCall)
    let genericArgs = genericParameterListFromClassType classType
    let methodContext = show classType ++ " " ++ showContext methodCall
    genericsMap <- bindGenericParameters methodContext genericParams genericArgs
    checkGetExpressionOnObject (show classType) genericsMap singletonObjectType methodCall

checkGetExpressionOnObject :: String -> GenericsMap -> ObjectType -> FunctionCall -> StaticCheckMonad ObjectType
checkGetExpressionOnObject context genericsMap objectType (CallFunction functionIdent ArgumentListAbsent) =
    checkGetExpressionOnObject context genericsMap objectType (CallFunction functionIdent (ArgumentListPresent []))
checkGetExpressionOnObject context genericsMap objectType (CallFunction functionIdent (ArgumentListPresent args)) = do
    argTypes <- checkArgumentList context args
    takeGetter <- hasGetterStatic objectType functionIdent
    if takeGetter then checkGetterCall context objectType functionIdent argTypes
    else checkMemberFunctionCall context genericsMap objectType functionIdent argTypes

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
    else returnVoid
    (predicateType, _) <- checkExpression predicateContext predicateExpr
    assertTypesMatch predicateContext boolType predicateType

checkDoExpression :: String -> DoExpr -> StaticCheckMonad ObjectType
checkDoExpression context (DoExpressionInstance objectIdent methodCall) = do
    objectType <- checkLocalObject objectIdent
    genericsMap <- genericsMapFromClassType $ classTypeFromObjectType objectType
    checkDoExpressionOnObject (showContext objectIdent) genericsMap objectType methodCall

checkDoExpression context (DoExpressionChain prefixGetExpression methodCall) = do
    (prefixObjectType, _) <- checkExpression context $ EGetExpression prefixGetExpression
    genericsMap <- genericsMapFromClassType $ classTypeFromObjectType prefixObjectType
    checkDoExpressionOnObject (showContext prefixGetExpression) genericsMap prefixObjectType methodCall

checkDoExpression context (DoExpressionStatic classType methodCall) = do
    (singletonObjectType, genericParams) <- checkStaticExpression classType (showContext methodCall)
    let genericArgs = genericParameterListFromClassType classType
    let methodContext = show classType ++ " " ++ showContext methodCall
    genericsMap <- bindGenericParameters methodContext genericParams genericArgs
    checkDoExpressionOnObject (show classType) genericsMap singletonObjectType methodCall

checkDoExpressionOnObject :: String -> GenericsMap -> ObjectType -> ActionCall -> StaticCheckMonad ObjectType
checkDoExpressionOnObject context genericsMap objectType (CallAction actionIdent ArgumentListAbsent) =
    checkDoExpressionOnObject context genericsMap objectType (CallAction actionIdent (ArgumentListPresent []))
checkDoExpressionOnObject context genericsMap objectType (CallAction actionIdent (ArgumentListPresent args)) = do
    argTypes <- checkArgumentList context args
    takeSetter <- hasSetterStatic objectType actionIdent
    if takeSetter then checkSetterCall context objectType actionIdent argTypes
    else checkMemberActionCall context genericsMap objectType actionIdent argTypes

checkLocalObjectDeclaration :: LocalDecl -> StaticCheckMonad StaticResult
checkLocalObjectDeclaration (LocalValueDeclaration objectDecl) =
    checkObjectDeclaration InitializedRequired False objectDecl
checkLocalObjectDeclaration (LocalVariableDeclaration objectDecl) =
    checkObjectDeclaration InitializedRequired True objectDecl

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

checkMemberFunctionCall :: String -> Map.Map ObjectType ObjectType -> ObjectType -> MethodIdent -> [ObjectType] -> StaticCheckMonad ObjectType
checkMemberFunctionCall context genericsMap objectType functionIdent argTypes = do
    let classType = classTypeFromObjectType objectType
    classDecl <- getClassDeclarationStatic classType
    let functionType = functionTypeFromClassDeclaration classDecl functionIdent
    when (isNothing functionType) $ throwError $ NoSuchFunctionError context (showContext functionIdent)
    let methodContext = context ++ "." ++ showContext functionIdent
    let builtinFunctionIdent = builtinMethodIdentifier functionIdent
    let implicitArgTypes = [stringType | builtinWithImplicitContext builtinFunctionIdent]
    if shouldHaveUniformTypes builtinFunctionIdent then checkTypeUniformity methodContext argTypes else returnVoid
    checkMethodArguments methodContext genericsMap (getMethodParamTypes $ fromJust functionType) (argTypes ++ implicitArgTypes)
    return $ mapTypeIfGeneric genericsMap $ getMethodReturnType $ fromJust functionType

checkMemberActionCall :: String -> Map.Map ObjectType ObjectType -> ObjectType -> MethodIdent -> [ObjectType] -> StaticCheckMonad ObjectType
checkMemberActionCall context genericsMap objectType actionIdent argTypes = do
    let classType = classTypeFromObjectType objectType
    classDecl <- getClassDeclarationStatic classType
    let actionType = actionTypeFromClassDeclaration classDecl actionIdent
    when (isNothing actionType) $ throwError $ NoSuchActionError context (showContext actionIdent)
    let methodContext = context ++ "#" ++ showContext actionIdent
    let builtinActionIdent = builtinMethodIdentifier actionIdent
    let implicitArgTypes = [stringType | builtinWithImplicitContext builtinActionIdent]
    if shouldHaveUniformTypes builtinActionIdent then checkTypeUniformity methodContext argTypes else returnVoid
    checkMethodArguments methodContext genericsMap (getMethodParamTypes $ fromJust actionType) (argTypes ++ implicitArgTypes)
    return $ mapTypeIfGeneric genericsMap $ getMethodReturnType $ fromJust actionType

checkMethodArguments :: String -> Map.Map ObjectType ObjectType -> [ObjectType] -> [ObjectType] -> StaticCheckMonad ObjectType
checkMethodArguments context genericsMap unmappedExpected actual = do
    let expected = map (mapTypeIfGeneric genericsMap) unmappedExpected
    let argumentTypesMatch = all (uncurry typesMatch) $ zip expected actual
    unless (length expected == length actual && argumentTypesMatch) $ throwError $
            MethodArgumentListInvalidError context (showContext expected) (showContext actual)
    returnVoid

checkStaticExpression :: ClassType -> String -> StaticCheckMonad (ObjectType, [ClassType])
checkStaticExpression classType callContext = do
    classDecl <- getClassDeclarationStatic classType
    unless (isSingletonClass classDecl) $ throwError $ NonSingletonClassError
        $ show classType ++ " " ++ callContext
    return (ObjectTypeClass classType, getGenericParameterList classDecl)
