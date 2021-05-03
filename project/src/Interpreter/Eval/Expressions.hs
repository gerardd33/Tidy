module Interpreter.Eval.Expressions where

import           Control.Monad.Reader
import           Data.Maybe

import           Interpreter.Common.Environment
import           Interpreter.Common.Types
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

evalLiteral :: Literal -> StateMonad Value
-- TODO NOW Pass "int" as constructor argument here
evalLiteral (LInt int)          = return (newSingleValueObject (IntValue int))
evalLiteral (LBool bool)        = return (newSingleValueObject (BoolValue bool))
evalLiteral (LChar char)        = return (newSingleValueObject (CharValue char))
evalLiteral (LString string)    = return (newSingleValueObject (StringValue string))
evalLiteral (LVoid void)        = return (newSingleValueObject VoidValue)

evalAddition :: Value -> Value -> StateMonad Value
evalAddition (SingleValueObject (IntValue v1)) (SingleValueObject (IntValue v2)) =
    return (newSingleValueObject (IntValue $ v1 + v2))

evalSubtraction :: Value -> Value -> StateMonad Value
evalSubtraction (SingleValueObject (IntValue v1)) (SingleValueObject (IntValue v2)) =
    return (newSingleValueObject (IntValue $ v1 - v2))

evalMultiplication :: Value -> Value -> StateMonad Value
evalMultiplication (SingleValueObject (IntValue v1)) (SingleValueObject (IntValue v2)) =
    return (newSingleValueObject (IntValue $ v1 * v2))

-- -- TODO throw if division by zero
evalDivision :: Value -> Value -> StateMonad Value
evalDivision (SingleValueObject (IntValue v1)) (SingleValueObject (IntValue v2)) =
    return (newSingleValueObject (IntValue $ v1 `div` v2))

declareValue :: ValueDeclProper -> StateMonad Result
declareValue (InitializedValue identifier valueType expr) = do
    (initializationValue, _) <- evalExpr expr
    addValue identifier initializationValue
