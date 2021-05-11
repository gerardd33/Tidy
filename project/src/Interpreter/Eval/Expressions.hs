module Interpreter.Eval.Expressions where

import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.List                         as List
import qualified Data.Map                          as Map
import           Data.Maybe
import           Data.Tuple

import           Interpreter.Common.Types
import           Parser.Tidy.Abs

import           Interpreter.Common.Helper.Classes
import           Interpreter.Common.Helper.Methods
import           Interpreter.Common.Helper.Objects


evaluateExpressionList :: [Expr] -> StateMonad Result
evaluateExpressionList [expr] = evalExpr expr
evaluateExpressionList (expr:exprs) = do
    (_, env) <- evalExpr expr
    local (const env) (evaluateExpressionList exprs)

evalExpr :: Expr -> StateMonad Result
evalExpr (ELiteral literal) = returnPure $ evalLiteral literal
evalExpr (ELocalValue identifier) = returnPure $ getValue identifier
evalExpr (ELocalValueDeclaration (LocalValueDeclaration (ObjectDeclaration _ declProper))) =
    declareValue declProper
evalExpr (EAdd expr1 expr2) = evalBinaryOperator expr1 expr2 evalAddition
evalExpr (ESubtract expr1 expr2) = evalBinaryOperator expr1 expr2 evalSubtraction
evalExpr (EMultiply expr1 expr2) = evalBinaryOperator expr1 expr2 evalMultiplication
evalExpr (EDivide expr1 expr2) = evalBinaryOperator expr1 expr2 evalMultiplication
evalExpr (EConcatenate expr1 expr2) = evalBinaryOperator expr1 expr2 evalConcatenation
evalExpr (EUnaryNot expr) = evalUnaryOperator expr evalUnaryNot
evalExpr (EUnaryMinus expr) = evalUnaryOperator expr evalUnaryMinus
evalExpr (ERelationalOperator expr1 operator expr2) = evalRelationalOperator expr1 expr2 operator
evalExpr (EBooleanOperator expr1 operator expr2) = evalBooleanOperator expr1 expr2 operator

evalExpr (EConstructorCall (CallConstructor classIdentifier argList)) = do
    (localEnv, classEnv) <- ask
    evaluatedArgs <- evalArgumentList argList
    let classDecl = classEnv Map.! classIdentifier
    let objectType = ObjectTypeClass classIdentifier GenericParameterAbsent
    initializedAttributes <- evalAttributeExpressions (getInitializedAttributeList classDecl)
    objectEnv <- buildObjectEnv objectType evaluatedArgs initializedAttributes
    let object = RegularObject objectType objectEnv
    return (object, (localEnv, classEnv))

evalExpr (EFunctionalControlFlow (FIfThenElse predicate thenBranch elseBranch)) = do
    (predicateValue, _) <- evalExpr predicate
    if isValueTrue predicateValue then evalThenBranch thenBranch else evalElseBranch elseBranch

evalExpr (EImperativeControlFlow (IWhile predicate body)) = do
    (predicateValue, _) <- evalExpr predicate
    if isValueTrue predicateValue
    then evaluateExpressionList body >> evalExpr (EImperativeControlFlow (IWhile predicate body))
    else returnPass

evalExpr (EImperativeControlFlow (IIf predicate body optionalElseBranch)) = do
    (predicateValue, _) <- evalExpr predicate
    if isValueTrue predicateValue then evaluateExpressionList body
    else case optionalElseBranch of
        IElseAbsent       -> returnPass
        IElsePresent body -> evaluateExpressionList body

evalExpr (EGetExpression (GetExpressionInstance objectIdentifier methodCall)) = do
    object <- getValue objectIdentifier
    evalGetExprOnObject object methodCall

evalExpr (EGetExpression (GetExpressionChain prefixGetExpr methodCall)) = do
    (prefixValue, _) <- evalExpr $ EGetExpression prefixGetExpr
    evalGetExprOnObject prefixValue methodCall

evalExpr (EGetExpression (GetExpressionStatic singletonClass methodCall)) = do
    (_, classEnv) <- ask
    singletonObject <- getValue $ singletonInstanceIdentifier singletonClass
    evalGetExprOnObject singletonObject methodCall


evalBinaryOperator :: Expr -> Expr -> (Value -> Value -> StateMonad Value) -> StateMonad Result
evalBinaryOperator expr1 expr2 evaluator = do
    (v1, _) <- evalExpr expr1
    (v2, _) <- evalExpr expr2
    returnPure $ evaluator v1 v2

evalUnaryOperator :: Expr -> (Value -> StateMonad Value) -> StateMonad Result
evalUnaryOperator expr evaluator = do
    (value, _) <- evalExpr expr
    returnPure $ evaluator value

evalLiteral :: Literal -> StateMonad Value
evalLiteral (LInt int)          = return (newSingleValueObject (IntValue int))
evalLiteral (LBool bool)        = return (newSingleValueObject (BoolValue bool))
evalLiteral (LChar char)        = return (newSingleValueObject (CharValue char))
evalLiteral (LString string)    = return (newSingleValueObject (StringValue string))
evalLiteral (LVoid void)        = return (newSingleValueObject VoidValue)

evalAddition :: Value -> Value -> StateMonad Value
evalAddition (SingleValueObject (IntValue v1)) (SingleValueObject (IntValue v2)) =
    return (newSingleValueObject $ IntValue $ v1 + v2)

evalSubtraction :: Value -> Value -> StateMonad Value
evalSubtraction (SingleValueObject (IntValue v1)) (SingleValueObject (IntValue v2)) =
    return (newSingleValueObject $ IntValue $ v1 - v2)

evalMultiplication :: Value -> Value -> StateMonad Value
evalMultiplication (SingleValueObject (IntValue v1)) (SingleValueObject (IntValue v2)) =
    return (newSingleValueObject $ IntValue $ v1 * v2)

-- TODO throw if division by zero
evalDivision :: Value -> Value -> StateMonad Value
evalDivision (SingleValueObject (IntValue v1)) (SingleValueObject (IntValue v2)) =
    return (newSingleValueObject $ IntValue $ v1 `div` v2)

evalConcatenation :: Value -> Value -> StateMonad Value
evalConcatenation (SingleValueObject (StringValue v1)) (SingleValueObject (StringValue v2)) =
    return (newSingleValueObject $ StringValue $ v1 ++ v2)

evalUnaryNot :: Value -> StateMonad Value
evalUnaryNot (SingleValueObject (BoolValue BTrue)) = return (newSingleValueObject $ BoolValue BFalse)
evalUnaryNot (SingleValueObject (BoolValue BFalse)) = return (newSingleValueObject $ BoolValue BTrue)

evalUnaryMinus :: Value -> StateMonad Value
evalUnaryMinus (SingleValueObject (IntValue value)) = return (newSingleValueObject $ IntValue $ -value)

evalRelationalOperator :: Expr -> Expr -> RelationalOperator -> StateMonad Result
evalRelationalOperator expr1 expr2 RLess = evalBinaryOperator expr1 expr2 (evalRelational (<))
evalRelationalOperator expr1 expr2 RLessEqual = evalBinaryOperator expr1 expr2 (evalRelational (<=))
evalRelationalOperator expr1 expr2 RGreater = evalBinaryOperator expr1 expr2 (evalRelational (>))
evalRelationalOperator expr1 expr2 RGreaterEqual = evalBinaryOperator expr1 expr2 (evalRelational (>=))
evalRelationalOperator expr1 expr2 REqual = evalBinaryOperator expr1 expr2 evalEquality
evalRelationalOperator expr1 expr2 RNotEqual = evalBinaryOperator expr1 expr2 evalNonEquality

evalRelational :: (Integer -> Integer -> Bool) -> Value -> Value -> StateMonad Value
evalRelational operator (SingleValueObject (IntValue v1)) (SingleValueObject (IntValue v2)) =
    return (newSingleValueObject $ BoolValue $ toBoolean $ operator v1 v2)

evalBooleanOperator expr1 expr2 BAnd = evalBinaryOperator expr1 expr2 evalBooleanAnd
evalBooleanOperator expr1 expr2 BOr = evalBinaryOperator expr1 expr2 evalBooleanOr

evalBooleanAnd :: Value -> Value -> StateMonad Value
evalBooleanAnd (SingleValueObject (BoolValue v1)) (SingleValueObject (BoolValue v2)) =
    return (newSingleValueObject $ BoolValue $ toBoolean $ fromBoolean v1 && fromBoolean v2)

evalBooleanOr :: Value -> Value -> StateMonad Value
evalBooleanOr (SingleValueObject (BoolValue v1)) (SingleValueObject (BoolValue v2)) =
    return (newSingleValueObject $ BoolValue $ toBoolean $ fromBoolean v1 || fromBoolean v2)

evalEquality :: Value -> Value -> StateMonad Value
evalEquality v1 v2 = return (newSingleValueObject $ BoolValue $ toBoolean $ v1 == v2)

evalNonEquality :: Value -> Value -> StateMonad Value
evalNonEquality v1 v2 = return (newSingleValueObject $ BoolValue $ toBoolean $ v1 /= v2)

addArgumentsToEnv :: FunctionDecl -> [Value] -> StateMonad Result
addArgumentsToEnv function evaluatedArgs = do
    (localEnv, classEnv) <- ask
    let methodParamsList = getMethodParamsList $ getFunctionType function
    let decls = zip methodParamsList evaluatedArgs
    (_, newEnv) <- executeValueAdditions decls
    return (pass, newEnv)

evalAttributeExpressions :: [(ObjectIdent, Expr)] -> StateMonad [(ObjectIdent, Value)]
evalAttributeExpressions attributeList = do
    let (names, exprs) = unzip attributeList
    evalResults <- mapM evalExpr exprs
    return $ zip names (map fst evalResults)

evalThenBranch :: ThenBranch -> StateMonad Result
evalThenBranch (FThenOneLine expr)   = evalThenBranch (FThenMultiLine expr)
evalThenBranch (FThenMultiLine expr) = evalExpr expr

evalElseBranch :: ElseBranch -> StateMonad Result
evalElseBranch (FElseOneLine expr) = evalElseBranch (FElseMultiLine expr)
evalElseBranch (FElseIf expr thenBranch elseBranch) =
    evalExpr (EFunctionalControlFlow (FIfThenElse expr thenBranch elseBranch))
evalElseBranch (FElseMultiLine expr) = evalExpr expr

evalFunction :: FunctionDecl -> StateMonad Result
evalFunction (FunctionDeclaration _ _ _ _ body) = evalFunctionBody body

evalFunctionBody :: FunctionBody -> StateMonad Result
evalFunctionBody (FunctionBodyOneLine expr)                    = evalExpr expr
evalFunctionBody (FunctionBodyMultiLine expr WithValuesAbsent) = evalExpr expr
evalFunctionBody (FunctionBodyMultiLine expr (WithValuesPresent ValuesAbsent)) = evalExpr expr
evalFunctionBody (FunctionBodyMultiLine expr (WithValuesPresent (ValuesPresent valueDecls))) = do
    originalEnv <- ask
    let decls = map getProperObjectDecl valueDecls
    (_, localEnv) <-  executeDeclarations decls
    (result, _) <- local (const localEnv) (evalExpr expr)
    return (result, originalEnv)

evalArgumentList :: ArgList -> StateMonad [Value]
evalArgumentList argList = do
    env <- ask
    evalResults <- mapM evalExpr (argsToExprList argList)
    return $ map fst evalResults

evalMemberFunction :: Value -> MethodIdent -> [Value] -> StateMonad Result
evalMemberFunction object functionIdentifier evaluatedArgs = do
    function <- getMemberFunction (getObjectType object) functionIdentifier
    (_, functionLocalEnv) <- addArgumentsToEnv function evaluatedArgs
    local (const functionLocalEnv) (evalFunction function)

evalGetter :: Value -> MethodIdent -> StateMonad Result
evalGetter (RegularObject _ objectEnv) functionIdentifier = do
    let attribute = functionToObjectIdent functionIdentifier
    if attribute `Map.member` values objectEnv
    then returnValue $ values objectEnv Map.! attribute
    else returnValue $ variables objectEnv Map.! attribute
-- TODO throw if single value object

executeDeclarations :: [ObjectDeclProper] -> StateMonad Result
executeDeclarations [] = returnPass
executeDeclarations (decl:decls) = do
    (_, env) <- declareValue decl
    local (const env) $ executeDeclarations decls

declareValue :: ObjectDeclProper -> StateMonad Result
declareValue (ObjectDeclarationProper identifier valueType (Initialized expression)) = do
    (initializationValue, _) <- evalExpr expression
    addValue identifier initializationValue

evalGetExprOnObject :: Value -> FunctionCall -> StateMonad Result
evalGetExprOnObject object (CallFunction functionIdentifier argList) = do
    originalEnv <- ask
    evaluatedArgs <- evalArgumentList argList
    takeGetter <- hasGetter (getObjectType object) functionIdentifier
    (result, _) <- if null evaluatedArgs && takeGetter
                 then evalGetter object functionIdentifier
                 else evalMemberFunction object functionIdentifier evaluatedArgs
    return (result, originalEnv)

toBoolean :: Bool -> Boolean
toBoolean True  = BTrue
toBoolean False = BFalse

fromBoolean :: Boolean -> Bool
fromBoolean BTrue  = True
fromBoolean BFalse = False

isValueTrue :: Value -> Bool
isValueTrue (SingleValueObject (BoolValue BTrue))  = True
isValueTrue (SingleValueObject (BoolValue BFalse)) = False
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
    (_, newEnv) <- executeValueAdditions $ zip objectNames singletonObjects
    return (pass, newEnv)

getLocation :: ObjectIdent -> StateMonad Location
getLocation identifier = do
    (localEnv, _) <- ask
    return $ fromJust $ Map.lookup identifier localEnv

getValue :: ObjectIdent -> StateMonad Value
getValue identifier = do
    location <- getLocation identifier
    (state, _) <- get
    return $ fromJust $ Map.lookup location state

setValue :: ObjectIdent -> Value -> StateMonad Result
setValue identifier value = do
    location <- getLocation identifier
    (state, nextLocation) <- get
    put (Map.insert location value state, nextLocation)
    returnPass

addValue :: ObjectIdent -> Value -> StateMonad Result
addValue identifier value = do
    (localEnv, classEnv) <- ask
    (state, nextLocation) <- get
    put (Map.insert nextLocation value state, nextLocation + 1)
    return (pass, (Map.insert identifier nextLocation localEnv, classEnv))

executeValueAdditions :: [(ObjectIdent, Value)] -> StateMonad Result
executeValueAdditions [] = returnPass
executeValueAdditions (addition:additions) = do
    (_, env) <- uncurry addValue addition
    local (const env) $ executeValueAdditions additions

returnPass :: StateMonad Result
returnPass = do
    env <- ask
    return (pass, env)

returnPure :: StateMonad Value -> StateMonad Result
returnPure function = do
    value <- function
    returnValue value

returnValue :: Value -> StateMonad Result
returnValue value = do
    env <- ask
    return (value, env)

buildSingletonClassInstance :: ClassDecl -> StateMonad Value
buildSingletonClassInstance (ClassDeclaration _ _ classIdentifier _ _) = do
    (localEnv, classEnv) <- ask
    let classDecl = classEnv Map.! classIdentifier
    let objectType = ObjectTypeClass classIdentifier GenericParameterAbsent
    initializedAttributes <- evalAttributeExpressions (getInitializedAttributeList classDecl)
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

buildObjectEnv :: ObjectType -> [Value] -> [(ObjectIdent, Value)] -> StateMonad ObjectEnv
buildObjectEnv objectType args initializedAttributes = do
    (_, classEnv) <- ask
    let classDecl = classEnv Map.! classIdentFromType objectType
    let ctorParamsList = getCtorParamsList classDecl
    let attributesFromCtor = Map.fromList $ zip ctorParamsList args
    let attributes = Map.union (Map.fromList initializedAttributes) attributesFromCtor
    objectValueList <- getValueList objectType
    let (values, variables) = Map.partitionWithKey (\name _ -> name `elem` objectValueList) attributes
    return $ ObjectEnv values variables
