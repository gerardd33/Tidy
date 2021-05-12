module Interpreter.Eval.Expressions.Evaluation where

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
import           Interpreter.Eval.Classes
import           Interpreter.Eval.Expressions.Miscellaneous
import           Interpreter.Eval.Expressions.Operators
import           Interpreter.Eval.LocalEnvironment
import           Interpreter.Eval.Methods
import           Interpreter.Eval.Utils


evaluateExpressionList :: [Expr] -> StateMonad Result
evaluateExpressionList [expr] = evaluateExpression expr
evaluateExpressionList (expr:exprs) = do
    (_, env) <- evaluateExpression expr
    local (const env) (evaluateExpressionList exprs)

evaluateExpression :: Expr -> StateMonad Result
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

evaluateExpression (EFunctionalControlFlow (FIfThenElse predicate thenBranch elseBranch)) =
    liftPure $ evaluateFunctionalIfThenElse predicate thenBranch elseBranch

evaluateExpression (EGetExpression (GetExpressionInstance objectIdent methodCall)) = do
    object <- getLocalValue objectIdent
    liftPure $ evaluateGetExpressionOnObject object methodCall

evaluateExpression (EGetExpression (GetExpressionChain prefixGetExpression methodCall)) = do
    (prefixObject, _) <- evaluateExpression $ EGetExpression prefixGetExpression
    liftPure $ evaluateGetExpressionOnObject prefixObject methodCall

evaluateExpression (EGetExpression (GetExpressionStatic singletonClass methodCall)) = do
    singletonObject <- getLocalValue $ singletonInstanceIdentifier singletonClass
    liftPure $ evaluateGetExpressionOnObject singletonObject methodCall

evaluateExpression (EConstructorCall (CallConstructor classIdent argList)) =
    liftPure $ evaluateConstructorCall classIdent argList

evaluateExpression (ELocalValueDeclaration (LocalValueDeclaration declaration)) =
    evaluateLocalValueDeclaration $ getProperDeclaration declaration

evaluateExpression (EImperativeControlFlow (IIf predicate body optionalElseBranch)) =
    evaluateImperativeIf predicate body optionalElseBranch

evaluateExpression (EImperativeControlFlow (IWhile predicate body)) = evaluateWhile predicate body


-- PURELY FUNCTIONAL EXPRESSIONS --
evaluateFunction :: FunctionDecl -> StateMonad Object
evaluateFunction = evaluateFunctionBody . getFunctionBody

evaluateFunctionBody :: FunctionBody -> StateMonad Object
evaluateFunctionBody (FunctionBodyOneLine expr) = returnPure $ evaluateExpression expr
evaluateFunctionBody (FunctionBodyMultiLine expr withValues) = returnPure $ case withValues of
    WithValuesPresent (ValuesPresent values) -> do let declarations = map getProperDeclaration values
                                                   (_, localEnv) <-  executeDeclarations declarations
                                                   local (const localEnv) $ evaluateExpression expr
    _ -> evaluateExpression expr

evaluateMemberFunction :: Object -> MethodIdent -> [Object] -> StateMonad Object
evaluateMemberFunction object functionIdent evaluatedArgs = do
    function <- getMemberFunction (getLocalObjectType object) functionIdent
    (_, functionLocalEnv) <- addArgumentsToEnv function evaluatedArgs
    local (const functionLocalEnv) $ evaluateFunction function

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

evaluateFunctionalIfThenElse :: Expr -> ThenBranch -> ElseBranch -> StateMonad Object
evaluateFunctionalIfThenElse predicate thenBranch elseBranch = do
   (predicateValue, _) <- evaluateExpression predicate
   if isTrue predicateValue
   then evaluateThenBranch thenBranch
   else evaluateElseBranch elseBranch

evaluateThenBranch :: ThenBranch -> StateMonad Object
evaluateThenBranch (FThenOneLine expr)   = returnPure $ evaluateExpression expr
evaluateThenBranch (FThenMultiLine expr) = returnPure $ evaluateExpression expr

evaluateElseBranch :: ElseBranch -> StateMonad Object
evaluateElseBranch (FElseOneLine expr) = returnPure $ evaluateExpression expr
evaluateElseBranch (FElseMultiLine expr) = returnPure $ evaluateExpression expr
evaluateElseBranch (FElseIf predicate thenBranch elseBranch) =
    returnPure $ evaluateExpression $ EFunctionalControlFlow $ FIfThenElse predicate thenBranch elseBranch

evaluateGetExpressionOnObject :: Object -> FunctionCall -> StateMonad Object
evaluateGetExpressionOnObject object (CallFunction functionIdent argumentList) = do
    originalEnv <- ask
    evaluatedArgs <- evaluateArgumentList argumentList
    takeGetter <- hasGetter (getLocalObjectType object) functionIdent
    if takeGetter && null evaluatedArgs
    then evaluateGetter object functionIdent
    else evaluateMemberFunction object functionIdent evaluatedArgs

evaluateConstructorCall :: ClassIdent -> ArgList -> StateMonad Object
evaluateConstructorCall classIdent argList = do
    classDecl <- getClassDecl classIdent
    evaluatedArgs <- evaluateArgumentList argList
    let objectType = ObjectTypeClass classIdent GenericParameterAbsent
    initializedAttributes <- evaluateAttributeExpressions (getInitializedAttributeList classDecl)
    objectEnv <- buildObjectEnv objectType evaluatedArgs initializedAttributes
    return $ RegularObject objectType objectEnv


-- EXPRESSIONS WITH SIDE EFFECTS --
-- TODO passing parameters and other context information
evaluateAction :: ActionDecl -> StateMonad Result
evaluateAction = evaluateActionBody . getActionBody

evaluateActionBody :: ActionBody -> StateMonad Result
evaluateActionBody (ActionBodyOneLine expr)    = evaluateExpressionList [expr]
evaluateActionBody (ActionBodyMultiLine exprs) = evaluateExpressionList exprs

evaluateLocalValueDeclaration :: ObjectDeclProper -> StateMonad Result
evaluateLocalValueDeclaration (ObjectDeclarationProper objectIdent objectType (Initialized expr)) = do
    (initializationValue, _) <- evaluateExpression expr
    addLocalValue objectIdent initializationValue

evaluateImperativeIf :: Expr -> [Expr] -> OptionalElseBranch -> StateMonad Result
evaluateImperativeIf predicate body optionalElseBranch = do
    (predicateValue, _) <- evaluateExpression predicate
    if isTrue predicateValue then evaluateExpressionList body
    else case optionalElseBranch of
        IElseAbsent       -> returnPass
        IElsePresent body -> evaluateExpressionList body
        IElseIf predicate body optionalElseBranch -> evaluateImperativeIf predicate body optionalElseBranch

evaluateWhile :: Expr -> [Expr] -> StateMonad Result
evaluateWhile predicate body  = do
    (predicateValue, _) <- evaluateExpression predicate
    if isTrue predicateValue
    then evaluateExpressionList body >> evaluateWhile predicate body
    else returnPass










buildSingletonClassInstance :: ClassDecl -> StateMonad Object
buildSingletonClassInstance (ClassDeclaration _ _ classIdentifier _ _) = do
    (localEnv, classEnv) <- ask
    let classDecl = classEnv Map.! classIdentifier
    let objectType = ObjectTypeClass classIdentifier GenericParameterAbsent
    initializedAttributes <- evaluateAttributeExpressions (getInitializedAttributeList classDecl)
    objectEnv <- buildObjectEnv objectType [] initializedAttributes
    return $ RegularObject objectType objectEnv


evaluateArgumentList :: ArgList -> StateMonad [Object]
evaluateArgumentList argList = do
    evalResults <- mapM evaluateExpression $ argsToExpressionList argList
    return $ map fst evalResults




executeDeclarations :: [ObjectDeclProper] -> StateMonad Result
executeDeclarations [] = returnPass
executeDeclarations (decl:decls) = do
    (_, env) <- evaluateLocalValueDeclaration decl
    local (const env) $ executeDeclarations decls



-- TODO refactor and move somewhere else

getValueList :: ObjectType -> StateMonad [ObjectIdent]
getValueList (ObjectTypeClass classIdent _) = do
    classDecl <- getClassDecl classIdent
    return $ getValues classDecl

buildObjectEnv :: ObjectType -> [Object] -> [(ObjectIdent, Object)] -> StateMonad ObjectEnv
buildObjectEnv objectType args initializedAttributes = do
    classDecl <- getClassDecl $ classFromObjectType objectType
    let ctorParamsList = getCtorParamsList classDecl
    let attributesFromCtor = Map.fromList $ zip ctorParamsList args
    let attributes = Map.union (Map.fromList initializedAttributes) attributesFromCtor
    objectValueList <- getValueList objectType
    let (values, variables) = Map.partitionWithKey (\name _ -> name `elem` objectValueList) attributes
    return $ ObjectEnv values variables



evaluateAttributeExpressions :: [(ObjectIdent, Expr)] -> StateMonad [(ObjectIdent, Object)]
evaluateAttributeExpressions attributeList = do
    let (names, exprs) = unzip attributeList
    evalResults <- mapM evaluateExpression exprs
    return $ zip names (map fst evalResults)
