module Interpreter.Eval.Expressions where

import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.Map                           as Map
import           Data.Maybe
import           Data.Tuple

import           Interpreter.Common.Types
import           Interpreter.Eval.Classes
import           Interpreter.Eval.Functions
import           Interpreter.Eval.Objects
import           Interpreter.Eval.ValueDeclarations
import           Parser.Tidy.Abs


evalExprList :: [Expr] -> StateMonad Result
evalExprList [expr] = evalExpr expr
evalExprList (expr:exprs) = do
    (_, env) <- evalExpr expr
    local (const env) (evalExprList exprs)

evalExpr :: Expr -> StateMonad Result
evalExpr (ELiteral literal) = returnPure $ evalLiteral literal
evalExpr (ELocalValue identifier) = returnPure $ getValue identifier
evalExpr (ELocalValueDecl (LocalVDecl (PublicValueDecl decl))) = declareValue decl
evalExpr (EAdd expr1 expr2) = evalBinaryOperator expr1 expr2 evalAddition
evalExpr (ESubtract expr1 expr2) = evalBinaryOperator expr1 expr2 evalSubtraction
evalExpr (EMultiply expr1 expr2) = evalBinaryOperator expr1 expr2 evalMultiplication
evalExpr (EDivide expr1 expr2) = evalBinaryOperator expr1 expr2 evalMultiplication
evalExpr (EConcatenate expr1 expr2) = evalBinaryOperator expr1 expr2 evalConcatenation
evalExpr (EUnaryNot expr) = evalUnaryOperator expr evalUnaryNot
evalExpr (EUnaryMinus expr) = evalUnaryOperator expr evalUnaryMinus
evalExpr (ERelationalOperator expr1 operator expr2) = evalRelationalOperator expr1 expr2 operator
evalExpr (EBooleanOperator expr1 operator expr2) = evalBooleanOperator expr1 expr2 operator

evalExpr (ECtorCall (CCall classIdentifier argList)) = do
    (localEnv, classEnv) <- ask
    evaluatedArgs <- evalArgumentList argList
    let classDecl = classEnv Map.! classIdentifier
    let objectType = ValueTypeClass classIdentifier
    initializedAttributes <- evalAttributeExpressions (getInitializedAttributeList classDecl)
    objectEnv <- buildObjectEnv objectType evaluatedArgs initializedAttributes
    object <- newRegularObject objectType objectEnv
    return (object, (localEnv, classEnv))

evalExpr (EFunctionalControlFlow (FIfThenElse predicate thenBranch elseBranch)) = do
    (predicateValue, _) <- evalExpr predicate
    if isValueTrue predicateValue then evalThenBranch thenBranch else evalElseBranch elseBranch

evalExpr (EImperativeControlFlow (IWhile predicate body)) = do
    (predicateValue, _) <- evalExpr predicate
    if isValueTrue predicateValue
    then evalExprList body >> evalExpr (EImperativeControlFlow (IWhile predicate body))
    else returnPass

evalExpr (EImperativeControlFlow (IIf predicate body optionalElseBranch)) = do
    (predicateValue, _) <- evalExpr predicate
    if isValueTrue predicateValue then evalExprList body
    else case optionalElseBranch of
        ElseAbsent       -> returnPass
        ElsePresent body -> evalExprList body

evalExpr (EGetExpr (GetExprInstance objectIdentifier methodCall)) = do
    object <- getValue objectIdentifier
    evalGetExprOnObject object methodCall

evalExpr (EGetExpr (GetExprChain prefixGetExpr methodCall)) = do
    (prefixValue, _) <- evalExpr $ EGetExpr prefixGetExpr
    evalGetExprOnObject prefixValue methodCall

evalExpr (EGetExpr (GetExprStatic singletonClass methodCall)) = do
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

evalAttributeExpressions :: [(ValueIdent, Expr)] -> StateMonad [(ValueIdent, Value)]
evalAttributeExpressions attributeList = do
    let (names, exprs) = unzip attributeList
    evalResults <- mapM evalExpr exprs
    return $ zip names (map fst evalResults)

evalThenBranch :: ThenBranch -> StateMonad Result
evalThenBranch (ThenOneLine expr)   = evalThenBranch (ThenMultiLine expr)
evalThenBranch (ThenMultiLine expr) = evalExpr expr

evalElseBranch :: ElseBranch -> StateMonad Result
evalElseBranch (ElseOneLine expr) = evalElseBranch (ElseMultiLine expr)
evalElseBranch (ElseIf expr thenBranch elseBranch) =
    evalExpr (EFunctionalControlFlow (FIfThenElse expr thenBranch elseBranch))
evalElseBranch (ElseMultiLine expr) = evalExpr expr

evalFunction :: FunctionDecl -> StateMonad Result
evalFunction (OverrideFunctionDecl _ _ body) = evalFunctionBody body
evalFunction (PublicFunctionDecl _ _ body)   = evalFunctionBody body
evalFunction (PrivateFunctionDecl _ _ body)  = evalFunctionBody body

evalFunctionBody :: FunctionBody -> StateMonad Result
evalFunctionBody (FunctionBodyOneLine expr)                    = evalExpr expr
evalFunctionBody (FunctionBodyMultiLine expr WithValuesAbsent) = evalExpr expr
evalFunctionBody (FunctionBodyMultiLine expr (WithValuesPresent ValuesAbsent)) = evalExpr expr
evalFunctionBody (FunctionBodyMultiLine expr (WithValuesPresent (ValuesPresent (ValuesSBody valueDecls)))) = do
    originalEnv <- ask
    let decls = map getProperValueDecl valueDecls
    (_, localEnv) <-  executeDeclarations decls
    (result, _) <- local (const localEnv) (evalExpr expr)
    return (result, originalEnv)

evalArgumentList :: ArgumentList -> StateMonad [Value]
evalArgumentList argList = do
    env <- ask
    evalResults <- mapM evalExpr (argsToExprList argList)
    return $ map fst evalResults

evalMemberFunction :: Value -> FunctionIdent -> [Value] -> StateMonad Result
evalMemberFunction object functionIdentifier evaluatedArgs = do
    function <- getMemberFunction (getObjectType object) functionIdentifier
    (_, functionLocalEnv) <- addArgumentsToEnv function evaluatedArgs
    local (const functionLocalEnv) (evalFunction function)

evalGetter :: Value -> FunctionIdent -> StateMonad Result
evalGetter (RegularObject _ objectEnv) functionIdentifier = do
    let attribute = functionToValueIdent functionIdentifier
    if attribute `Map.member` values objectEnv
    then returnValue $ values objectEnv Map.! attribute
    else returnValue $ variables objectEnv Map.! attribute
-- TODO throw if single value object

executeDeclarations :: [ValueDeclProper] -> StateMonad Result
executeDeclarations [] = returnPass
executeDeclarations (decl:decls) = do
    (_, env) <- declareValue decl
    local (const env) $ executeDeclarations decls

declareValue :: ValueDeclProper -> StateMonad Result
declareValue (InitializedValue identifier valueType expr) = do
    (initializationValue, _) <- evalExpr expr
    addValue identifier initializationValue

evalGetExprOnObject :: Value -> FunctionCall -> StateMonad Result
evalGetExprOnObject object (FCall functionIdentifier argList) = do
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

buildInitialEnv :: ClassEnv -> Env
buildInitialEnv classEnv = (Map.empty, classEnv)

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

getLocation :: ValueIdent -> StateMonad Location
getLocation identifier = do
    (localEnv, _) <- ask
    return $ fromJust $ Map.lookup identifier localEnv

getValue :: ValueIdent -> StateMonad Value
getValue identifier = do
    location <- getLocation identifier
    (state, _) <- get
    return $ fromJust $ Map.lookup location state

setValue :: ValueIdent -> Value -> StateMonad Result
setValue identifier value = do
    location <- getLocation identifier
    (state, nextLocation) <- get
    put (Map.insert location value state, nextLocation)
    returnPass

addValue :: ValueIdent -> Value -> StateMonad Result
addValue identifier value = do
    (localEnv, classEnv) <- ask
    (state, nextLocation) <- get
    put (Map.insert nextLocation value state, nextLocation + 1)
    return (pass, (Map.insert identifier nextLocation localEnv, classEnv))

executeValueAdditions :: [(ValueIdent, Value)] -> StateMonad Result
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
buildSingletonClassInstance (ClassDeclConcrete _ classIdentifier _ _) = do
    (localEnv, classEnv) <- ask
    let classDecl = classEnv Map.! classIdentifier
    let objectType = ValueTypeClass classIdentifier
    initializedAttributes <- evalAttributeExpressions (getInitializedAttributeList classDecl)
    objectEnv <- buildObjectEnv objectType [] initializedAttributes
    newRegularObject objectType objectEnv
