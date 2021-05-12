module Interpreter.Eval.Expressions.Miscellaneous where

import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.List                         as List
import qualified Data.Map                          as Map
import           Data.Maybe

import           Interpreter.Common.Types
import           Parser.Tidy.Abs

import           Interpreter.Common.Helper.Classes
import           Interpreter.Common.Helper.Methods
import           Interpreter.Common.Helper.Objects
import           Interpreter.Eval.Utils


evalLiteral :: Literal -> StateMonad Object
evalLiteral (LInt int)          = return (newBuiltinObjectObject (IntObject int))
evalLiteral (LBool bool)        = return (newBuiltinObjectObject (BoolObject bool))
evalLiteral (LChar char)        = return (newBuiltinObjectObject (CharObject char))
evalLiteral (LString string)    = return (newBuiltinObjectObject (StringObject string))
evalLiteral (LVoid void)        = return (newBuiltinObjectObject VoidObject)







evalBinaryOperator :: Expr -> Expr -> (Object -> Object -> StateMonad Object) -> StateMonad Result
evalBinaryOperator expr1 expr2 evaluator = do
    (v1, _) <- evaluateExpression expr1
    (v2, _) <- evaluateExpression expr2
    returnPure $ evaluator v1 v2

evalUnaryOperator :: Expr -> (Object -> StateMonad Object) -> StateMonad Result
evalUnaryOperator expr evaluator = do
    (value, _) <- evaluateExpression expr
    returnPure $ evaluator value

evalAddition :: Object -> Object -> StateMonad Object
evalAddition (BuiltinObjectObject (IntObject v1)) (BuiltinObjectObject (IntObject v2)) =
    return (newBuiltinObjectObject $ IntObject $ v1 + v2)

evalSubtraction :: Object -> Object -> StateMonad Object
evalSubtraction (BuiltinObjectObject (IntObject v1)) (BuiltinObjectObject (IntObject v2)) =
    return (newBuiltinObjectObject $ IntObject $ v1 - v2)

evalMultiplication :: Object -> Object -> StateMonad Object
evalMultiplication (BuiltinObjectObject (IntObject v1)) (BuiltinObjectObject (IntObject v2)) =
    return (newBuiltinObjectObject $ IntObject $ v1 * v2)

-- TODO throw if division by zero
evalDivision :: Object -> Object -> StateMonad Object
evalDivision (BuiltinObjectObject (IntObject v1)) (BuiltinObjectObject (IntObject v2)) =
    return (newBuiltinObjectObject $ IntObject $ v1 `div` v2)

evalConcatenation :: Object -> Object -> StateMonad Object
evalConcatenation (BuiltinObjectObject (StringObject v1)) (BuiltinObjectObject (StringObject v2)) =
    return (newBuiltinObjectObject $ StringObject $ v1 ++ v2)

evalUnaryNot :: Object -> StateMonad Object
evalUnaryNot (BuiltinObjectObject (BoolObject BTrue)) = return (newBuiltinObjectObject $ BoolObject BFalse)
evalUnaryNot (BuiltinObjectObject (BoolObject BFalse)) = return (newBuiltinObjectObject $ BoolObject BTrue)

evalUnaryMinus :: Object -> StateMonad Object
evalUnaryMinus (BuiltinObjectObject (IntObject value)) = return (newBuiltinObjectObject $ IntObject $ -value)

evalRelationalOperator :: Expr -> Expr -> RelationalOperator -> StateMonad Result
evalRelationalOperator expr1 expr2 RLess = evalBinaryOperator expr1 expr2 (evalRelational (<))
evalRelationalOperator expr1 expr2 RLessEqual = evalBinaryOperator expr1 expr2 (evalRelational (<=))
evalRelationalOperator expr1 expr2 RGreater = evalBinaryOperator expr1 expr2 (evalRelational (>))
evalRelationalOperator expr1 expr2 RGreaterEqual = evalBinaryOperator expr1 expr2 (evalRelational (>=))
evalRelationalOperator expr1 expr2 REqual = evalBinaryOperator expr1 expr2 evalEquality
evalRelationalOperator expr1 expr2 RNotEqual = evalBinaryOperator expr1 expr2 evalNonEquality

evalRelational :: (Integer -> Integer -> Bool) -> Object -> Object -> StateMonad Object
evalRelational operator (BuiltinObjectObject (IntObject v1)) (BuiltinObjectObject (IntObject v2)) =
    return (newBuiltinObjectObject $ BoolObject $ toBoolean $ operator v1 v2)

evalBooleanOperator expr1 expr2 BAnd = evalBinaryOperator expr1 expr2 evalBooleanAnd
evalBooleanOperator expr1 expr2 BOr = evalBinaryOperator expr1 expr2 evalBooleanOr

evalBooleanAnd :: Object -> Object -> StateMonad Object
evalBooleanAnd (BuiltinObjectObject (BoolObject v1)) (BuiltinObjectObject (BoolObject v2)) =
    return (newBuiltinObjectObject $ BoolObject $ toBoolean $ fromBoolean v1 && fromBoolean v2)

evalBooleanOr :: Object -> Object -> StateMonad Object
evalBooleanOr (BuiltinObjectObject (BoolObject v1)) (BuiltinObjectObject (BoolObject v2)) =
    return (newBuiltinObjectObject $ BoolObject $ toBoolean $ fromBoolean v1 || fromBoolean v2)

evalEquality :: Object -> Object -> StateMonad Object
evalEquality v1 v2 = return (newBuiltinObjectObject $ BoolObject $ toBoolean $ v1 == v2)

evalNonEquality :: Object -> Object -> StateMonad Object
evalNonEquality v1 v2 = return (newBuiltinObjectObject $ BoolObject $ toBoolean $ v1 /= v2)

addArgumentsToEnv :: FunctionDecl -> [Object] -> StateMonad Result
addArgumentsToEnv function evaluatedArgs = do
    (localEnv, classEnv) <- ask
    let methodParamsList = getMethodParamsList $ getFunctionType function
    let decls = zip methodParamsList evaluatedArgs
    (_, newEnv) <- executeObjectAdditions decls
    return (pass, newEnv)

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

evalFunction :: FunctionDecl -> StateMonad Result
evalFunction (FunctionDeclaration _ _ _ _ body) = evalFunctionBody body

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
isObjectTrue (BuiltinObjectObject (BoolObject BTrue))  = True
isObjectTrue (BuiltinObjectObject (BoolObject BFalse)) = False
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
