module Interpreter.Eval.Expressions.Evaluate where

import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.List                                  as List
import qualified Data.Map                                   as Map
import           Data.Maybe

import           Interpreter.Common.Types
import           Parser.Tidy.Abs

import           Interpreter.Common.Helper.Classes
import           Interpreter.Common.Helper.Methods
import           Interpreter.Common.Helper.Objects
import           Interpreter.Eval.Environment
import           Interpreter.Eval.Expressions.Miscellaneous
import           Interpreter.Eval.Utils


evaluateExpressionList :: [Expr] -> StateMonad Result
evaluateExpressionList [expr] = evaluateExpression expr
evaluateExpressionList (expr:exprs) = do
    (_, env) <- evaluateExpression expr
    local (const env) (evaluateExpressionList exprs)

evaluateExpression :: Expr -> StateMonad Result
evaluateExpression (ELiteral literal) = returnPure $ evaluateLiteral literal
evaluateExpression (ELocalValue identifier) = returnPure $ getLocalValue identifier
evaluateExpression (ELocalValueDeclaration (LocalValueDeclaration declaration)) =
    declareLocalValue $ getProperDeclaration declaration

evaluateExpression (EAdd expr1 expr2) = evaluateBinaryOperator expr1 expr2 evaluateAddition
evaluateExpression (ESubtract expr1 expr2) = evaluateBinaryOperator expr1 expr2 evaluateSubtraction
evaluateExpression (EMultiply expr1 expr2) = evaluateBinaryOperator expr1 expr2 evaluateMultiplication
evaluateExpression (EDivide expr1 expr2) = evaluateBinaryOperator expr1 expr2 evaluateMultiplication
evaluateExpression (EConcatenate expr1 expr2) = evaluateBinaryOperator expr1 expr2 evaluateConcatenation
evaluateExpression (EUnaryNot expr) = evaluateUnaryOperator expr evaluateUnaryNot
evaluateExpression (EUnaryMinus expr) = evaluateUnaryOperator expr evaluateUnaryMinus
evaluateExpression (ERelationalOperator expr1 operator expr2) = evaluateRelationalOperator expr1 expr2 operator
evaluateExpression (EBooleanOperator expr1 operator expr2) = evaluateBooleanOperator expr1 expr2 operator

evaluateExpression (EConstructorCall (CallConstructor classIdentifier argList)) = do
    (localEnv, classEnv) <- ask
    evaluatedArgs <- evaluateArgumentList argList
    let classDecl = classEnv Map.! classIdentifier
    let objectType = ObjectTypeClass classIdentifier GenericParameterAbsent
    initializedAttributes <- evaluateAttributeExpressions (getInitializedAttributeList classDecl)
    objectEnv <- buildObjectEnv objectType evaluatedArgs initializedAttributes
    let object = RegularObject objectType objectEnv
    return (object, (localEnv, classEnv))

evaluateExpression (EFunctionalControlFlow (FIfThenElse predicate thenBranch elseBranch)) = do
    (predicateValue, _) <- evaluateExpression predicate
    if isObjectTrue predicateValue then evaluateThenBranch thenBranch else evaluateElseBranch elseBranch

evaluateExpression (EImperativeControlFlow (IWhile predicate body)) = do
    (predicateValue, _) <- evaluateExpression predicate
    if isObjectTrue predicateValue
    then evaluateExpressionList body >> evaluateExpression (EImperativeControlFlow (IWhile predicate body))
    else returnPass

evaluateExpression (EImperativeControlFlow (IIf predicate body optionalElseBranch)) = do
    (predicateValue, _) <- evaluateExpression predicate
    if isObjectTrue predicateValue then evaluateExpressionList body
    else case optionalElseBranch of
        IElseAbsent       -> returnPass
        IElsePresent body -> evaluateExpressionList body
-- TODO elif

evaluateExpression (EGetExpression (GetExpressionInstance objectIdentifier methodCall)) = do
    object <- getLocalValue objectIdentifier
    evaluateGetExprOnObject object methodCall

evaluateExpression (EGetExpression (GetExpressionChain prefixGetExpr methodCall)) = do
    (prefixValue, _) <- evaluateExpression $ EGetExpression prefixGetExpr
    evaluateGetExprOnObject prefixValue methodCall

evaluateExpression (EGetExpression (GetExpressionStatic singletonClass methodCall)) = do
    (_, classEnv) <- ask
    singletonObject <- getLocalValue $ singletonInstanceIdentifier singletonClass
    evaluateGetExprOnObject singletonObject methodCall





evaluateBinaryOperator :: Expr -> Expr -> (Object -> Object -> StateMonad Object) -> StateMonad Result
evaluateBinaryOperator expr1 expr2 evaluator = do
    (v1, _) <- evaluateExpression expr1
    (v2, _) <- evaluateExpression expr2
    returnPure $ evaluator v1 v2

evaluateUnaryOperator :: Expr -> (Object -> StateMonad Object) -> StateMonad Result
evaluateUnaryOperator expr evaluator = do
    (value, _) <- evaluateExpression expr
    returnPure $ evaluator value

evaluateAttributeExpressions :: [(ObjectIdent, Expr)] -> StateMonad [(ObjectIdent, Object)]
evaluateAttributeExpressions attributeList = do
    let (names, exprs) = unzip attributeList
    evalResults <- mapM evaluateExpression exprs
    return $ zip names (map fst evalResults)

evaluateThenBranch :: ThenBranch -> StateMonad Result
evaluateThenBranch (FThenOneLine expr)   = evaluateThenBranch (FThenMultiLine expr)
evaluateThenBranch (FThenMultiLine expr) = evaluateExpression expr

evaluateElseBranch :: ElseBranch -> StateMonad Result
evaluateElseBranch (FElseOneLine expr) = evaluateElseBranch (FElseMultiLine expr)
evaluateElseBranch (FElseIf expr thenBranch elseBranch) =
    evaluateExpression (EFunctionalControlFlow (FIfThenElse expr thenBranch elseBranch))
evaluateElseBranch (FElseMultiLine expr) = evaluateExpression expr


evaluateFunctionBody :: FunctionBody -> StateMonad Result
evaluateFunctionBody (FunctionBodyOneLine expr)                    = evaluateExpression expr
evaluateFunctionBody (FunctionBodyMultiLine expr WithValuesAbsent) = evaluateExpression expr
evaluateFunctionBody (FunctionBodyMultiLine expr (WithValuesPresent ValuesAbsent)) = evaluateExpression expr
evaluateFunctionBody (FunctionBodyMultiLine expr (WithValuesPresent (ValuesPresent valueDecls))) = do
    originalEnv <- ask
    let decls = map getProperDeclaration valueDecls
    (_, localEnv) <-  executeDeclarations decls
    (result, _) <- local (const localEnv) (evaluateExpression expr)
    return (result, originalEnv)

evaluateArgumentList :: ArgList -> StateMonad [Object]
evaluateArgumentList argList = do
    env <- ask
    evalResults <- mapM evaluateExpression (argsToExprList argList)
    return $ map fst evalResults

evaluateAddition :: Object -> Object -> StateMonad Object
evaluateAddition (BuiltinObject (IntObject v1)) (BuiltinObject (IntObject v2)) =
    return (newBuiltinObject $ IntObject $ v1 + v2)

evaluateSubtraction :: Object -> Object -> StateMonad Object
evaluateSubtraction (BuiltinObject (IntObject v1)) (BuiltinObject (IntObject v2)) =
    return (newBuiltinObject $ IntObject $ v1 - v2)

evaluateMultiplication :: Object -> Object -> StateMonad Object
evaluateMultiplication (BuiltinObject (IntObject v1)) (BuiltinObject (IntObject v2)) =
    return (newBuiltinObject $ IntObject $ v1 * v2)

-- TODO throw if division by zero
evaluateDivision :: Object -> Object -> StateMonad Object
evaluateDivision (BuiltinObject (IntObject v1)) (BuiltinObject (IntObject v2)) =
    return (newBuiltinObject $ IntObject $ v1 `div` v2)

evaluateConcatenation :: Object -> Object -> StateMonad Object
evaluateConcatenation (BuiltinObject (StringObject v1)) (BuiltinObject (StringObject v2)) =
    return (newBuiltinObject $ StringObject $ v1 ++ v2)

evaluateUnaryNot :: Object -> StateMonad Object
evaluateUnaryNot (BuiltinObject (BoolObject BTrue)) = return (newBuiltinObject $ BoolObject BFalse)
evaluateUnaryNot (BuiltinObject (BoolObject BFalse)) = return (newBuiltinObject $ BoolObject BTrue)

evaluateUnaryMinus :: Object -> StateMonad Object
evaluateUnaryMinus (BuiltinObject (IntObject value)) = return (newBuiltinObject $ IntObject $ -value)

evaluateRelationalOperator :: Expr -> Expr -> RelationalOperator -> StateMonad Result
evaluateRelationalOperator expr1 expr2 RLess = evaluateBinaryOperator expr1 expr2 (evaluateRelational (<))
evaluateRelationalOperator expr1 expr2 RLessEqual = evaluateBinaryOperator expr1 expr2 (evaluateRelational (<=))
evaluateRelationalOperator expr1 expr2 RGreater = evaluateBinaryOperator expr1 expr2 (evaluateRelational (>))
evaluateRelationalOperator expr1 expr2 RGreaterEqual = evaluateBinaryOperator expr1 expr2 (evaluateRelational (>=))
evaluateRelationalOperator expr1 expr2 REqual = evaluateBinaryOperator expr1 expr2 evaluateEquality
evaluateRelationalOperator expr1 expr2 RNotEqual = evaluateBinaryOperator expr1 expr2 evaluateNonEquality

evaluateRelational :: (Integer -> Integer -> Bool) -> Object -> Object -> StateMonad Object
evaluateRelational operator (BuiltinObject (IntObject v1)) (BuiltinObject (IntObject v2)) =
    return (newBuiltinObject $ BoolObject $ toBoolean $ operator v1 v2)

evaluateBooleanOperator expr1 expr2 BAnd = evaluateBinaryOperator expr1 expr2 evaluateBooleanAnd
evaluateBooleanOperator expr1 expr2 BOr = evaluateBinaryOperator expr1 expr2 evaluateBooleanOr

evaluateBooleanAnd :: Object -> Object -> StateMonad Object
evaluateBooleanAnd (BuiltinObject (BoolObject v1)) (BuiltinObject (BoolObject v2)) =
    return (newBuiltinObject $ BoolObject $ toBoolean $ fromBoolean v1 && fromBoolean v2)

evaluateBooleanOr :: Object -> Object -> StateMonad Object
evaluateBooleanOr (BuiltinObject (BoolObject v1)) (BuiltinObject (BoolObject v2)) =
    return (newBuiltinObject $ BoolObject $ toBoolean $ fromBoolean v1 || fromBoolean v2)

evaluateEquality :: Object -> Object -> StateMonad Object
evaluateEquality v1 v2 = return (newBuiltinObject $ BoolObject $ toBoolean $ v1 == v2)

evaluateNonEquality :: Object -> Object -> StateMonad Object
evaluateNonEquality v1 v2 = return (newBuiltinObject $ BoolObject $ toBoolean $ v1 /= v2)

addArgumentsToEnv :: FunctionDecl -> [Object] -> StateMonad Result
addArgumentsToEnv function evaluatedArgs = do
    (localEnv, classEnv) <- ask
    let methodParamsList = getMethodParamsList $ getFunctionType function
    let decls = zip methodParamsList evaluatedArgs
    (_, newEnv) <- executeObjectAdditions decls
    return (pass, newEnv)

evaluateFunction :: FunctionDecl -> StateMonad Result
evaluateFunction (FunctionDeclaration _ _ _ _ body) = evaluateFunctionBody body


evaluateMemberFunction :: Object -> MethodIdent -> [Object] -> StateMonad Result
evaluateMemberFunction object functionIdentifier evaluatedArgs = do
    function <- getMemberFunction (getLocalValueType object) functionIdentifier
    (_, functionLocalEnv) <- addArgumentsToEnv function evaluatedArgs
    local (const functionLocalEnv) (evaluateFunction function)

evaluateGetter :: Object -> MethodIdent -> StateMonad Result
evaluateGetter (RegularObject _ objectEnv) functionIdentifier = do
    let attribute = functionToObjectIdent functionIdentifier
    if attribute `Map.member` values objectEnv
    then returnObject $ values objectEnv Map.! attribute
    else returnObject $ variables objectEnv Map.! attribute
-- TODO throw if single value object

executeDeclarations :: [ObjectDeclProper] -> StateMonad Result
executeDeclarations [] = returnPass
executeDeclarations (decl:decls) = do
    (_, env) <- declareLocalValue decl
    local (const env) $ executeDeclarations decls

declareLocalValue :: ObjectDeclProper -> StateMonad Result
declareLocalValue (ObjectDeclarationProper identifier valueType (Initialized expression)) = do
    (initializationObject, _) <- evaluateExpression expression
    addObject identifier initializationObject

evaluateGetExprOnObject :: Object -> FunctionCall -> StateMonad Result
evaluateGetExprOnObject object (CallFunction functionIdentifier argList) = do
    originalEnv <- ask
    evaluatedArgs <- evaluateArgumentList argList
    takeGetter <- hasGetter (getLocalValueType object) functionIdentifier
    (result, _) <- if null evaluatedArgs && takeGetter
                 then evaluateGetter object functionIdentifier
                 else evaluateMemberFunction object functionIdentifier evaluatedArgs
    return (result, originalEnv)

toBoolean :: Bool -> Boolean
toBoolean True  = BTrue
toBoolean False = BFalse

fromBoolean :: Boolean -> Bool
fromBoolean BTrue  = True
fromBoolean BFalse = False

isObjectTrue :: Object -> Bool
isObjectTrue (BuiltinObject (BoolObject BTrue))  = True
isObjectTrue (BuiltinObject (BoolObject BFalse)) = False
-- TODO exception in other cases

buildInitialEnvironment :: ClassEnv -> Env
buildInitialEnvironment classEnv = (Map.empty, classEnv)

buildInitialState :: RTState
buildInitialState = (Map.empty, 0)

buildInitialLocalEnv :: ClassEnv -> StateMonad Result
buildInitialLocalEnv classEnv = do
    let singletonClasses = Map.toList $ Map.filter isSingletonClass classEnv
    let (classNames, classDecls) = unzip singletonClasses
    let objectNames = map singletonInstanceIdentifier classNames
    singletonObjects <- mapM buildSingletonClassInstance classDecls
    (_, newEnv) <- executeObjectAdditions $ zip objectNames singletonObjects
    return (pass, newEnv)

setObject :: ObjectIdent -> Object -> StateMonad Result
setObject identifier value = do
    location <- getLocation identifier
    (state, nextLocation) <- get
    put (Map.insert location value state, nextLocation)
    returnPass

addObject :: ObjectIdent -> Object -> StateMonad Result
addObject identifier value = do
    (localEnv, classEnv) <- ask
    (state, nextLocation) <- get
    put (Map.insert nextLocation value state, nextLocation + 1)
    return (pass, (Map.insert identifier nextLocation localEnv, classEnv))

executeObjectAdditions :: [(ObjectIdent, Object)] -> StateMonad Result
executeObjectAdditions [] = returnPass
executeObjectAdditions (addition:additions) = do
    (_, env) <- uncurry addObject addition
    local (const env) $ executeObjectAdditions additions

buildSingletonClassInstance :: ClassDecl -> StateMonad Object
buildSingletonClassInstance (ClassDeclaration _ _ classIdentifier _ _) = do
    (localEnv, classEnv) <- ask
    let classDecl = classEnv Map.! classIdentifier
    let objectType = ObjectTypeClass classIdentifier GenericParameterAbsent
    initializedAttributes <- evaluateAttributeExpressions (getInitializedAttributeList classDecl)
    objectEnv <- buildObjectEnv objectType [] initializedAttributes
    return $ RegularObject objectType objectEnv



-- TODO refactor and move somewhere else

getValueList :: ObjectType -> StateMonad [ObjectIdent]
getValueList (ObjectTypeClass className _) = do
    (_, classEnv) <- ask
    return $ getValues $ classEnv Map.! className

getMemberFunction :: ObjectType -> MethodIdent -> StateMonad FunctionDecl
getMemberFunction (ObjectTypeClass className _) functionIdentifier = do
    (_, classEnv) <- ask
    let functions = getFunctionDecls $ classEnv Map.! className
    return $ fromJust $ List.find (\f -> getFunctionName f == functionIdentifier) functions

hasGetter :: ObjectType -> MethodIdent -> StateMonad Bool
hasGetter objectType functionIdentifier = do
    (_, classEnv) <- ask
    let classDecl = classEnv Map.! classIdentFromType objectType
    let attributeIdentifier = functionToObjectIdent functionIdentifier
    let attributes = getValues classDecl ++ getVariables classDecl
    return $ attributeIdentifier `elem` attributes

buildObjectEnv :: ObjectType -> [Object] -> [(ObjectIdent, Object)] -> StateMonad ObjectEnv
buildObjectEnv objectType args initializedAttributes = do
    (_, classEnv) <- ask
    let classDecl = classEnv Map.! classIdentFromType objectType
    let ctorParamsList = getCtorParamsList classDecl
    let attributesFromCtor = Map.fromList $ zip ctorParamsList args
    let attributes = Map.union (Map.fromList initializedAttributes) attributesFromCtor
    objectValueList <- getValueList objectType
    let (values, variables) = Map.partitionWithKey (\name _ -> name `elem` objectValueList) attributes
    return $ ObjectEnv values variables
