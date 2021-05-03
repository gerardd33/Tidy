module Interpreter.Eval.Expressions where

import           Control.Monad.Reader
import qualified Data.Map                     as Map
import           Data.Maybe
import           Data.Tuple

import           Interpreter.Common.Types
import           Interpreter.Eval.Classes
import           Interpreter.Eval.Environment
import           Interpreter.Eval.Functions
import           Interpreter.Eval.Objects
import           Parser.Tidy.Abs


evalExpressionList :: [Expr] -> StateMonad Result
evalExpressionList [expr] = evalExpr expr
evalExpressionList (expr:exprs) = do
    (_, env) <- evalExpr expr
    local (const env) (evalExpressionList exprs)

evalExpr :: Expr -> StateMonad Result
evalExpr (ELiteral literal) = returnPure $ evalLiteral literal

evalExpr (ELocalValue identifier) = returnPure $ getValue identifier
evalExpr (ELocalValueDecl (LocalVDecl (PublicValueDecl decl))) = declareValue decl

evalExpr (EAdd expr1 expr2) = do
    (v1, _) <- evalExpr expr1
    (v2, _) <- evalExpr expr2
    returnPure $ evalAddition v1 v2

evalExpr (ESubtract expr1 expr2) = do
    (v1, _) <- evalExpr expr1
    (v2, _) <- evalExpr expr2
    returnPure $ evalSubtraction v1 v2

evalExpr (EMultiply expr1 expr2) = do
    (v1, _) <- evalExpr expr1
    (v2, _) <- evalExpr expr2
    returnPure $ evalMultiplication v1 v2

evalExpr (EDivide expr1 expr2) = do
    (v1, _) <- evalExpr expr1
    (v2, _) <- evalExpr expr2
    returnPure $ evalDivision v1 v2

evalExpr (EUnaryNot expr) = do
    (value, _) <- evalExpr expr
    returnPure $ evalUnaryNot value

evalExpr (EUnaryMinus expr) = do
    (value, _) <- evalExpr expr
    returnPure $ evalUnaryMinus value

evalExpr (ECtorCall (CCall classIdentifier args)) = do
    env <- ask
    evalResults <- mapM evalExpr (argsToExprList args)
    let evaluatedArgs = map fst evalResults
    let objectType = ValueTypeClass classIdentifier
    objectEnv <- buildObjectEnv objectType evaluatedArgs
    object <- newRegularObject objectType objectEnv
    return (object, env)


evalLiteral :: Literal -> StateMonad Value
-- TODO NOW Pass "int" as constructor argument here
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

-- -- TODO throw if division by zero
evalDivision :: Value -> Value -> StateMonad Value
evalDivision (SingleValueObject (IntValue v1)) (SingleValueObject (IntValue v2)) =
    return (newSingleValueObject $ IntValue $ v1 `div` v2)

evalUnaryNot :: Value -> StateMonad Value
evalUnaryNot (SingleValueObject (BoolValue value)) = return (newSingleValueObject $ BoolValue $ not value)

evalUnaryMinus :: Value -> StateMonad Value
evalUnaryMinus (SingleValueObject (IntValue value)) = return (newSingleValueObject $ BoolValue $ -value)

declareValue :: ValueDeclProper -> StateMonad Result
declareValue (InitializedValue identifier valueType expr) = do
    (initializationValue, _) <- evalExpr expr
    addValue identifier initializationValue

buildObjectEnv :: ValueType -> [Value] -> StateMonad ObjectEnv
buildObjectEnv objectType args = do
    (_, classEnv) <- ask
    let classDecl = classEnv Map.! classIdentFromType objectType
    let ctorArgsList = getCtorArgsList classDecl
    initializedAttributes <- evalAttributeExpressions (getInitializedAttributeList classDecl)
    let attributesFromCtor = Map.fromList $ zip ctorArgsList args
    let attributes = Map.union (Map.fromList initializedAttributes) attributesFromCtor
    objectValueList <- getValueList objectType
    let (values, variables) = Map.partitionWithKey (\name _ -> name `elem` objectValueList) attributes
    return $ ObjectEnv values variables

evalAttributeExpressions :: [(ValueIdent, Expr)] -> StateMonad [(ValueIdent, Value)]
evalAttributeExpressions attributeList = do
    let (names, exprs) = unzip attributeList
    evalResults <- mapM evalExpr exprs
    return $ zip names (map fst evalResults)
