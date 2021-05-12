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
import           Interpreter.Common.Helper.Types
import           Interpreter.Eval.Expressions.Miscellaneous
import           Interpreter.Eval.Expressions.Operators
import           Interpreter.Eval.LocalEnvironment
import           Interpreter.Eval.Utils


evaluateExpressionList :: [Expr] -> StateMonad Result
evaluateExpressionList [expr] = evaluateExpression expr
evaluateExpressionList (expr:exprs) = do
    (_, env) <- evaluateExpression expr
    local (const env) (evaluateExpressionList exprs)

evaluateExpression :: Expr -> StateMonad Result

-- PURELY FUNCTIONAL EXPRESSIONS --
evaluateExpression (ELiteral literal) = liftPure $ evaluateLiteral literal
evaluateExpression (ELocalValue identifier) = liftPure $ getLocalValue identifier
evaluateExpression (EAdd expr1 expr2) = liftPure $ evaluateBinaryOperator expr1 expr2 evaluateAddition
evaluateExpression (ESubtract expr1 expr2) = liftPure $ evaluateBinaryOperator expr1 expr2 evaluateSubtraction
evaluateExpression (EMultiply expr1 expr2) = liftPure $ evaluateBinaryOperator expr1 expr2 evaluateMultiplication
evaluateExpression (EDivide expr1 expr2) = liftPure $ evaluateBinaryOperator expr1 expr2 evaluateMultiplication
evaluateExpression (EModulo expr1 expr2) = liftPure $ evaluateBinaryOperator expr1 expr2 evaluateModulo
evaluateExpression (EConcatenate expr1 expr2) = liftPure $ evaluateBinaryOperator expr1 expr2 evaluateConcatenation
evaluateExpression (EUnaryNot expr) = liftPure $ evaluateUnaryOperator expr evaluateUnaryNot
evaluateExpression (EUnaryMinus expr) = liftPure $ evaluateUnaryOperator expr evaluateUnaryMinus
evaluateExpression (ERelationalOperator expr1 operator expr2) = liftPure $ evaluateRelationalOperator expr1 expr2 operator
evaluateExpression (EBooleanOperator expr1 operator expr2) = liftPure $ evaluateBooleanOperator expr1 expr2 operator

evaluateExpression (EFunctionalControlFlow (FIfThenElse predicate thenBranch elseBranch)) = do
    (predicateValue, _) <- evaluateExpression predicate
    liftPure $ if isTrue predicateValue
    then evaluateThenBranch thenBranch
    else evaluateElseBranch elseBranch




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

evaluateExpression (EConstructorCall (CallConstructor classIdentifier argList)) = do
    (localEnv, classEnv) <- ask
    evaluatedArgs <- evaluateArgumentList argList
    let classDecl = classEnv Map.! classIdentifier
    let objectType = ObjectTypeClass classIdentifier GenericParameterAbsent
    initializedAttributes <- evaluateAttributeExpressions (getInitializedAttributeList classDecl)
    objectEnv <- buildObjectEnv objectType evaluatedArgs initializedAttributes
    let object = RegularObject objectType objectEnv
    return (object, (localEnv, classEnv))


-- EXPRESSIONS WITH SIDE EFFECTS --
evaluateExpression (ELocalValueDeclaration (LocalValueDeclaration declaration)) =
    declareLocalValue $ getProperDeclaration declaration

evaluateExpression (EImperativeControlFlow (IWhile predicate body)) = do
    (predicateValue, _) <- evaluateExpression predicate
    if isTrue predicateValue
    then evaluateExpressionList body >> evaluateExpression (EImperativeControlFlow (IWhile predicate body))
    else returnPass

evaluateExpression (EImperativeControlFlow (IIf predicate body optionalElseBranch)) = do
    (predicateValue, _) <- evaluateExpression predicate
    if isTrue predicateValue then evaluateExpressionList body
    else case optionalElseBranch of
        IElseAbsent       -> returnPass
        IElsePresent body -> evaluateExpressionList body
-- TODO elif


-- PURELY FUNCTIONAL EXPRESSIONS --
evaluateBinaryOperator :: Expr -> Expr -> (Object -> Object -> StateMonad Object) -> StateMonad Object
evaluateBinaryOperator expr1 expr2 evaluator = do
    (value1, _) <- evaluateExpression expr1
    (value2, _) <- evaluateExpression expr2
    evaluator value1 value2

evaluateUnaryOperator :: Expr -> (Object -> StateMonad Object) -> StateMonad Object
evaluateUnaryOperator expr evaluator = do
    (value, _) <- evaluateExpression expr
    evaluator value

evaluateRelationalOperator :: Expr -> Expr -> RelationalOperator -> StateMonad Object
evaluateRelationalOperator expr1 expr2 operator = do
    let evaluator = case operator of RLess         -> evaluateRelational (<)
                                     RLessEqual    -> evaluateRelational (<=)
                                     RGreater      -> evaluateRelational (>)
                                     RGreaterEqual -> evaluateRelational (>=)
                                     REqual        -> evaluateEquality
                                     RNotEqual     -> evaluateNonEquality
    evaluateBinaryOperator expr1 expr2 evaluator

evaluateBooleanOperator :: Expr -> Expr -> BooleanOperator -> StateMonad Object
evaluateBooleanOperator expr1 expr2 operator = do
    let evaluator = case operator of BAnd -> evaluateBooleanAnd
                                     BOr  -> evaluateBooleanOr
    evaluateBinaryOperator expr1 expr2 evaluator

evaluateThenBranch :: ThenBranch -> StateMonad Object
evaluateThenBranch (FThenOneLine expr)   = returnPure $ evaluateExpression expr
evaluateThenBranch (FThenMultiLine expr) = returnPure $ evaluateExpression expr

evaluateElseBranch :: ElseBranch -> StateMonad Object
evaluateElseBranch (FElseOneLine expr) = returnPure $ evaluateExpression expr
evaluateElseBranch (FElseMultiLine expr) = returnPure $ evaluateExpression expr
evaluateElseBranch (FElseIf predicate thenBranch elseBranch) =
    returnPure $ evaluateExpression $ EFunctionalControlFlow $ FIfThenElse predicate thenBranch elseBranch


-- EXPRESSIONS WITH SIDE EFFECTS --
declareLocalValue :: ObjectDeclProper -> StateMonad Result
declareLocalValue (ObjectDeclarationProper objectName objectType (Initialized expr)) = do
    (initializationValue, _) <- evaluateExpression expr
    addLocalValue objectName initializationValue














evaluateAttributeExpressions :: [(ObjectIdent, Expr)] -> StateMonad [(ObjectIdent, Object)]
evaluateAttributeExpressions attributeList = do
    let (names, exprs) = unzip attributeList
    evalResults <- mapM evaluateExpression exprs
    return $ zip names (map fst evalResults)


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

evaluateGetExprOnObject :: Object -> FunctionCall -> StateMonad Result
evaluateGetExprOnObject object (CallFunction functionIdentifier argList) = do
    originalEnv <- ask
    evaluatedArgs <- evaluateArgumentList argList
    takeGetter <- hasGetter (getLocalValueType object) functionIdentifier
    (result, _) <- if null evaluatedArgs && takeGetter
                 then evaluateGetter object functionIdentifier
                 else evaluateMemberFunction object functionIdentifier evaluatedArgs
    return (result, originalEnv)



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

executeObjectAdditions :: [(ObjectIdent, Object)] -> StateMonad Result
executeObjectAdditions [] = returnPass
executeObjectAdditions (addition:additions) = do
    (_, env) <- uncurry addLocalValue addition
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
