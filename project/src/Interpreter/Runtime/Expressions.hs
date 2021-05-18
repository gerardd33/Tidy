module Interpreter.Runtime.Expressions where

import           Control.Monad.Except
import           Interpreter.Common.Errors

import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.List                        as List
import qualified Data.Map                         as Map
import           Data.Maybe

import           Interpreter.Common.Types
import           Parser.Tidy.Abs

import           Interpreter.Common.Utils.Classes
import           Interpreter.Common.Utils.Methods
import           Interpreter.Common.Utils.Objects
import           Interpreter.Common.Utils.Types
import           Interpreter.Runtime.Classes
import           Interpreter.Runtime.Environments
import           Interpreter.Runtime.Methods
import           Interpreter.Runtime.Objects
import           Interpreter.Runtime.Operators
import           Interpreter.Runtime.Types


evaluateExpressionList :: [Expr] -> StateMonad Result
evaluateExpressionList [] = returnPass
evaluateExpressionList [expr] = evaluateExpression expr
evaluateExpressionList (expr:exprs) = do
    (_, env) <- evaluateExpression expr
    local (const env) $ evaluateExpressionList exprs

evaluateExpression :: Expr -> StateMonad Result
evaluateExpression (ELiteral literal) = liftPure $ evaluateLiteral literal
evaluateExpression (ELocalValue identifier) = liftPure $ getLocalObject identifier
evaluateExpression (EAdd expr1 expr2) = liftPure $ evaluateBinaryOperator expr1 expr2 evaluateAddition
evaluateExpression (ESubtract expr1 expr2) = liftPure $ evaluateBinaryOperator expr1 expr2 evaluateSubtraction
evaluateExpression (EMultiply expr1 expr2) = liftPure $ evaluateBinaryOperator expr1 expr2 evaluateMultiplication
evaluateExpression (EDivide expr1 expr2) = liftPure $ evaluateBinaryOperator expr1 expr2 evaluateDivision
evaluateExpression (EModulo expr1 expr2) = liftPure $ evaluateBinaryOperator expr1 expr2 evaluateModulo
evaluateExpression (EConcatenate expr1 expr2) = liftPure $ evaluateBinaryOperator expr1 expr2 evaluateConcatenation
evaluateExpression (EUnaryNot expr) = liftPure $ evaluateUnaryOperator expr evaluateUnaryNot
evaluateExpression (EUnaryMinus expr) = liftPure $ evaluateUnaryOperator expr evaluateUnaryMinus
evaluateExpression (ERelationalOperator expr1 operator expr2) = liftPure $ evaluateRelationalOperator expr1 expr2 operator
evaluateExpression (EBooleanOperator expr1 operator expr2) = liftPure $ evaluateBooleanOperator expr1 expr2 operator

evaluateExpression (EFunctionalControlFlow (FIfThenElse predicate thenBranch elseBranch)) =
    liftPure $ evaluateFunctionalIfThenElse predicate thenBranch elseBranch

evaluateExpression (EGetExpression (GetExpressionInstance objectIdent methodCall)) = do
    object <- getLocalObject objectIdent
    liftPure $ evaluateGetExpressionOnObject object methodCall

evaluateExpression (EGetExpression (GetExpressionChain prefixGetExpression methodCall)) = do
    (prefixObject, _) <- evaluateExpression $ EGetExpression prefixGetExpression
    liftPure $ evaluateGetExpressionOnObject prefixObject methodCall

evaluateExpression (EGetExpression (GetExpressionStatic singletonClass methodCall)) = do
    singletonObject <- getLocalObject $ singletonInstanceIdentifier singletonClass
    liftPure $ evaluateGetExpressionOnObject singletonObject methodCall

evaluateExpression (EConstructorCall (CallConstructor classIdent argList)) =
    liftPure $ evaluateConstructorCall classIdent argList

evaluateExpression (EDoExpression (DoExpressionInstance objectIdent methodCall)) = do
    object <- getLocalObject objectIdent
    evaluateDoExpressionOnObject object methodCall

evaluateExpression (EDoExpression (DoExpressionChain prefixGetExpression methodCall)) = do
    (prefixObject, _) <- evaluateExpression $ EGetExpression prefixGetExpression
    evaluateDoExpressionOnObject prefixObject methodCall

evaluateExpression (EDoExpression (DoExpressionStatic singletonClass methodCall)) = do
    singletonObject <- getLocalObject $ singletonInstanceIdentifier singletonClass
    evaluateDoExpressionOnObject singletonObject methodCall

evaluateExpression (ELocalDeclaration (LocalValueDeclaration declaration)) =
    evaluateLocalValueDeclaration $ getProperDeclaration declaration

evaluateExpression (ELocalDeclaration (LocalVariableDeclaration declaration)) =
    evaluateLocalVariableDeclaration $ getProperDeclaration declaration

evaluateExpression (EImperativeControlFlow (IIf predicate body optionalElseBranch)) =
    evaluateImperativeIf predicate body optionalElseBranch

evaluateExpression (EImperativeControlFlow (IWhile predicate body)) = evaluateWhile predicate body


-- PURELY FUNCTIONAL EXPRESSIONS --
evaluateFunctionInEnv :: FunctionDecl -> StateMonad Object
evaluateFunctionInEnv = evaluateFunctionBody . getFunctionBody

evaluateFunctionBody :: FunctionBody -> StateMonad Object
evaluateFunctionBody (FunctionBodyOneLine expr) = returnPure $ evaluateExpression expr
evaluateFunctionBody (FunctionBodyMultiLine expr withValues) = returnPure $ case withValues of
    WithValuesPresent (ValuesPresent values) -> do let declarations = map getProperDeclaration values
                                                   (_, methodEnv) <-  evaluateLocalValueDeclarations declarations
                                                   local (const methodEnv) $ evaluateExpression expr
    _ -> evaluateExpression expr

evaluateMemberFunction :: Object -> MethodIdent -> [Object] -> StateMonad Object
evaluateMemberFunction object functionIdent evaluatedArgs = do
    function <- getMemberFunction (getObjectType object) functionIdent
    (_, newEnv) <- setThisReference object
    (_, methodLocalEnv) <- local (const newEnv) $ addArgumentsToEnv (getFunctionType function) evaluatedArgs
    local (const methodLocalEnv) $ evaluateFunctionInEnv function

evaluateMemberAction :: Object -> MethodIdent -> [Object] -> StateMonad Result
evaluateMemberAction object actionIdent evaluatedArgs = do
    action <- getMemberAction (getObjectType object) actionIdent
    (_, newEnv) <- setThisReference object
    (_, actionMethodEnv) <- local (const newEnv) $ addArgumentsToEnv (getActionType action) evaluatedArgs
    local (const actionMethodEnv) $ evaluateActionInEnv action

evaluateBinaryOperator :: Expr -> Expr -> (Object -> Object -> Expr -> Expr -> StateMonad Object) -> StateMonad Object
evaluateBinaryOperator expr1 expr2 evaluator = do
    (value1, _) <- evaluateExpression expr1
    (value2, _) <- evaluateExpression expr2
    evaluator value1 value2 expr1 expr2

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
evaluateGetExpressionOnObject object (CallFunction functionIdent argList) = do
    originalEnv <- ask
    evaluatedArgs <- evaluateArgumentList argList
    takeGetter <- hasGetter (getObjectType object) functionIdent
    if takeGetter then evaluateGetter object functionIdent
    else evaluateMemberFunction object functionIdent evaluatedArgs

evaluateDoExpressionOnObject :: Object -> ActionCall -> StateMonad Result
evaluateDoExpressionOnObject object (CallAction actionIdent argList) = do
    originalEnv <- ask
    evaluatedArgs <- evaluateArgumentList argList
    takeSetter <- hasSetter (getObjectType object) actionIdent
    if takeSetter then evaluateSetter object actionIdent $ head evaluatedArgs
    else evaluateMemberAction object actionIdent evaluatedArgs

evaluateConstructorCall :: ClassIdent -> ArgList -> StateMonad Object
evaluateConstructorCall classIdent argList = do
    let objectType = ObjectTypeClass classIdent GenericParameterAbsent
    classDecl <- getClassDeclaration classIdent
    evaluatedArgs <- evaluateArgumentList argList
    initializedAttributes <- evaluateAttributeExpressions $ getInitializedAttributes classDecl
    objectEnv <- buildObjectEnv objectType evaluatedArgs initializedAttributes
    return $ RegularObject objectType objectEnv

evaluateArgumentList :: ArgList -> StateMonad [Object]
evaluateArgumentList argList = do
    evalResults <- mapM evaluateExpression $ argsToExpressionList argList
    return $ map fst evalResults

evaluateAttributeExpressions :: [(ObjectIdent, Expr)] -> StateMonad [(ObjectIdent, Object)]
evaluateAttributeExpressions attributeList = do
    let (names, exprs) = unzip attributeList
    evalResults <- mapM evaluateExpression exprs
    return $ zip names (map fst evalResults)

evaluateInitializedAttributes :: ClassDecl -> StateMonad [(ObjectIdent, Object)]
evaluateInitializedAttributes classDecl = evaluateAttributeExpressions $ getInitializedAttributes classDecl


-- EXPRESSIONS WITH SIDE EFFECTS --
evaluateActionInEnv :: ActionDecl -> StateMonad Result
evaluateActionInEnv = evaluateActionBody . getActionBody

evaluateActionBody :: ActionBody -> StateMonad Result
evaluateActionBody (ActionBodyOneLine expr)    = evaluateExpressionList [expr]
evaluateActionBody (ActionBodyMultiLine exprs) = evaluateExpressionList exprs

evaluateLocalValueDeclaration :: ObjectDeclProper -> StateMonad Result
evaluateLocalValueDeclaration (ObjectDeclarationProper objectIdent objectType (Initialized expr)) = do
    (initializationValue, _) <- evaluateExpression expr
    addLocalValue objectIdent initializationValue

evaluateLocalVariableDeclaration :: ObjectDeclProper -> StateMonad Result
evaluateLocalVariableDeclaration (ObjectDeclarationProper objectIdent objectType (Initialized expr)) = do
    (initializationValue, _) <- evaluateExpression expr
    addLocalVariable objectIdent initializationValue

evaluateLocalValueDeclarations :: [ObjectDeclProper] -> StateMonad Result
evaluateLocalValueDeclarations [] = returnPass
evaluateLocalValueDeclarations (decl:decls) = do
    (_, env) <- evaluateLocalValueDeclaration decl
    local (const env) $ evaluateLocalValueDeclarations decls

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
