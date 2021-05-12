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
import           Interpreter.Eval.Expressions.Miscellaneous
import           Interpreter.Eval.Utils


evaluateExpressionList :: [Expr] -> StateMonad Result
evaluateExpressionList [expr] = evaluateExpression expr
evaluateExpressionList (expr:exprs) = do
    (_, env) <- evaluateExpression expr
    local (const env) (evaluateExpressionList exprs)


evaluateExpression :: Expr -> StateMonad Result
evaluateExpression (ELiteral literal) = returnPure $ evalLiteral literal
evaluateExpression (ELocalValue identifier) = returnPure $ getObject identifier
evaluateExpression (ELocalValueDeclaration (LocalValueDeclaration (ObjectDeclaration _ declProper))) =
    declareObject declProper
evaluateExpression (EAdd expr1 expr2) = evalBinaryOperator expr1 expr2 evalAddition
evaluateExpression (ESubtract expr1 expr2) = evalBinaryOperator expr1 expr2 evalSubtraction
evaluateExpression (EMultiply expr1 expr2) = evalBinaryOperator expr1 expr2 evalMultiplication
evaluateExpression (EDivide expr1 expr2) = evalBinaryOperator expr1 expr2 evalMultiplication
evaluateExpression (EConcatenate expr1 expr2) = evalBinaryOperator expr1 expr2 evalConcatenation
evaluateExpression (EUnaryNot expr) = evalUnaryOperator expr evalUnaryNot
evaluateExpression (EUnaryMinus expr) = evalUnaryOperator expr evalUnaryMinus
evaluateExpression (ERelationalOperator expr1 operator expr2) = evalRelationalOperator expr1 expr2 operator
evaluateExpression (EBooleanOperator expr1 operator expr2) = evalBooleanOperator expr1 expr2 operator

evaluateExpression (EConstructorCall (CallConstructor classIdentifier argList)) = do
    (localEnv, classEnv) <- ask
    evaluatedArgs <- evalArgumentList argList
    let classDecl = classEnv Map.! classIdentifier
    let objectType = ObjectTypeClass classIdentifier GenericParameterAbsent
    initializedAttributes <- evalAttributeExpressions (getInitializedAttributeList classDecl)
    objectEnv <- buildObjectEnv objectType evaluatedArgs initializedAttributes
    let object = RegularObject objectType objectEnv
    return (object, (localEnv, classEnv))

evaluateExpression (EFunctionalControlFlow (FIfThenElse predicate thenBranch elseBranch)) = do
    (predicateValue, _) <- evaluateExpression predicate
    if isObjectTrue predicateValue then evalThenBranch thenBranch else evalElseBranch elseBranch

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
    object <- getObject objectIdentifier
    evalGetExprOnObject object methodCall

evaluateExpression (EGetExpression (GetExpressionChain prefixGetExpr methodCall)) = do
    (prefixValue, _) <- evaluateExpression $ EGetExpression prefixGetExpr
    evalGetExprOnObject prefixValue methodCall

evaluateExpression (EGetExpression (GetExpressionStatic singletonClass methodCall)) = do
    (_, classEnv) <- ask
    singletonObject <- getObject $ singletonInstanceIdentifier singletonClass
    evalGetExprOnObject singletonObject methodCall





evalBinaryOperator :: Expr -> Expr -> (Object -> Object -> StateMonad Object) -> StateMonad Result
evalBinaryOperator expr1 expr2 evaluator = do
    (v1, _) <- evaluateExpression expr1
    (v2, _) <- evaluateExpression expr2
    returnPure $ evaluator v1 v2

evalUnaryOperator :: Expr -> (Object -> StateMonad Object) -> StateMonad Result
evalUnaryOperator expr evaluator = do
    (value, _) <- evaluateExpression expr
    returnPure $ evaluator value

evalAttributeExpressions :: [(ObjectIdent, Expr)] -> StateMonad [(ObjectIdent, Object)]
evalAttributeExpressions attributeList = do
    let (names, exprs) = unzip attributeList
    evalResults <- mapM evaluateExpression exprs
    return $ zip names (map fst evalResults)

evalThenBranch :: ThenBranch -> StateMonad Result
evalThenBranch (FThenOneLine expr)   = evalThenBranch (FThenMultiLine expr)
evalThenBranch (FThenMultiLine expr) = evaluateExpression expr

evalElseBranch :: ElseBranch -> StateMonad Result
evalElseBranch (FElseOneLine expr) = evalElseBranch (FElseMultiLine expr)
evalElseBranch (FElseIf expr thenBranch elseBranch) =
    evaluateExpression (EFunctionalControlFlow (FIfThenElse expr thenBranch elseBranch))
evalElseBranch (FElseMultiLine expr) = evaluateExpression expr


evalFunctionBody :: FunctionBody -> StateMonad Result
evalFunctionBody (FunctionBodyOneLine expr)                    = evaluateExpression expr
evalFunctionBody (FunctionBodyMultiLine expr WithValuesAbsent) = evaluateExpression expr
evalFunctionBody (FunctionBodyMultiLine expr (WithValuesPresent ValuesAbsent)) = evaluateExpression expr
evalFunctionBody (FunctionBodyMultiLine expr (WithValuesPresent (ValuesPresent valueDecls))) = do
    originalEnv <- ask
    let decls = map getProperObjectDecl valueDecls
    (_, localEnv) <-  executeDeclarations decls
    (result, _) <- local (const localEnv) (evaluateExpression expr)
    return (result, originalEnv)

evalArgumentList :: ArgList -> StateMonad [Object]
evalArgumentList argList = do
    env <- ask
    evalResults <- mapM evaluateExpression (argsToExprList argList)
    return $ map fst evalResults

evalAddition :: Object -> Object -> StateMonad Object
evalAddition (BuiltinObject (IntObject v1)) (BuiltinObject (IntObject v2)) =
    return (newBuiltinObject $ IntObject $ v1 + v2)

evalSubtraction :: Object -> Object -> StateMonad Object
evalSubtraction (BuiltinObject (IntObject v1)) (BuiltinObject (IntObject v2)) =
    return (newBuiltinObject $ IntObject $ v1 - v2)

evalMultiplication :: Object -> Object -> StateMonad Object
evalMultiplication (BuiltinObject (IntObject v1)) (BuiltinObject (IntObject v2)) =
    return (newBuiltinObject $ IntObject $ v1 * v2)

-- TODO throw if division by zero
evalDivision :: Object -> Object -> StateMonad Object
evalDivision (BuiltinObject (IntObject v1)) (BuiltinObject (IntObject v2)) =
    return (newBuiltinObject $ IntObject $ v1 `div` v2)

evalConcatenation :: Object -> Object -> StateMonad Object
evalConcatenation (BuiltinObject (StringObject v1)) (BuiltinObject (StringObject v2)) =
    return (newBuiltinObject $ StringObject $ v1 ++ v2)

evalUnaryNot :: Object -> StateMonad Object
evalUnaryNot (BuiltinObject (BoolObject BTrue)) = return (newBuiltinObject $ BoolObject BFalse)
evalUnaryNot (BuiltinObject (BoolObject BFalse)) = return (newBuiltinObject $ BoolObject BTrue)

evalUnaryMinus :: Object -> StateMonad Object
evalUnaryMinus (BuiltinObject (IntObject value)) = return (newBuiltinObject $ IntObject $ -value)

evalRelationalOperator :: Expr -> Expr -> RelationalOperator -> StateMonad Result
evalRelationalOperator expr1 expr2 RLess = evalBinaryOperator expr1 expr2 (evalRelational (<))
evalRelationalOperator expr1 expr2 RLessEqual = evalBinaryOperator expr1 expr2 (evalRelational (<=))
evalRelationalOperator expr1 expr2 RGreater = evalBinaryOperator expr1 expr2 (evalRelational (>))
evalRelationalOperator expr1 expr2 RGreaterEqual = evalBinaryOperator expr1 expr2 (evalRelational (>=))
evalRelationalOperator expr1 expr2 REqual = evalBinaryOperator expr1 expr2 evalEquality
evalRelationalOperator expr1 expr2 RNotEqual = evalBinaryOperator expr1 expr2 evalNonEquality

evalRelational :: (Integer -> Integer -> Bool) -> Object -> Object -> StateMonad Object
evalRelational operator (BuiltinObject (IntObject v1)) (BuiltinObject (IntObject v2)) =
    return (newBuiltinObject $ BoolObject $ toBoolean $ operator v1 v2)

evalBooleanOperator expr1 expr2 BAnd = evalBinaryOperator expr1 expr2 evalBooleanAnd
evalBooleanOperator expr1 expr2 BOr = evalBinaryOperator expr1 expr2 evalBooleanOr

evalBooleanAnd :: Object -> Object -> StateMonad Object
evalBooleanAnd (BuiltinObject (BoolObject v1)) (BuiltinObject (BoolObject v2)) =
    return (newBuiltinObject $ BoolObject $ toBoolean $ fromBoolean v1 && fromBoolean v2)

evalBooleanOr :: Object -> Object -> StateMonad Object
evalBooleanOr (BuiltinObject (BoolObject v1)) (BuiltinObject (BoolObject v2)) =
    return (newBuiltinObject $ BoolObject $ toBoolean $ fromBoolean v1 || fromBoolean v2)

evalEquality :: Object -> Object -> StateMonad Object
evalEquality v1 v2 = return (newBuiltinObject $ BoolObject $ toBoolean $ v1 == v2)

evalNonEquality :: Object -> Object -> StateMonad Object
evalNonEquality v1 v2 = return (newBuiltinObject $ BoolObject $ toBoolean $ v1 /= v2)

addArgumentsToEnv :: FunctionDecl -> [Object] -> StateMonad Result
addArgumentsToEnv function evaluatedArgs = do
    (localEnv, classEnv) <- ask
    let methodParamsList = getMethodParamsList $ getFunctionType function
    let decls = zip methodParamsList evaluatedArgs
    (_, newEnv) <- executeObjectAdditions decls
    return (pass, newEnv)

evalFunction :: FunctionDecl -> StateMonad Result
evalFunction (FunctionDeclaration _ _ _ _ body) = evalFunctionBody body


evalMemberFunction :: Object -> MethodIdent -> [Object] -> StateMonad Result
evalMemberFunction object functionIdentifier evaluatedArgs = do
    function <- getMemberFunction (getObjectType object) functionIdentifier
    (_, functionLocalEnv) <- addArgumentsToEnv function evaluatedArgs
    local (const functionLocalEnv) (evalFunction function)

evalGetter :: Object -> MethodIdent -> StateMonad Result
evalGetter (RegularObject _ objectEnv) functionIdentifier = do
    let attribute = functionToObjectIdent functionIdentifier
    if attribute `Map.member` values objectEnv
    then returnObject $ values objectEnv Map.! attribute
    else returnObject $ variables objectEnv Map.! attribute
-- TODO throw if single value object

executeDeclarations :: [ObjectDeclProper] -> StateMonad Result
executeDeclarations [] = returnPass
executeDeclarations (decl:decls) = do
    (_, env) <- declareObject decl
    local (const env) $ executeDeclarations decls

declareObject :: ObjectDeclProper -> StateMonad Result
declareObject (ObjectDeclarationProper identifier valueType (Initialized expression)) = do
    (initializationObject, _) <- evaluateExpression expression
    addObject identifier initializationObject

evalGetExprOnObject :: Object -> FunctionCall -> StateMonad Result
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

getLocation :: ObjectIdent -> StateMonad Location
getLocation identifier = do
    (localEnv, _) <- ask
    return $ fromJust $ Map.lookup identifier localEnv

getObject :: ObjectIdent -> StateMonad Object
getObject identifier = do
    location <- getLocation identifier
    (state, _) <- get
    return $ fromJust $ Map.lookup location state

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
