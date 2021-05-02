module Interpreter.Eval.Expressions where

import           Control.Monad.Reader
import           Data.Maybe

import           Interpreter.Common.Environment
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
evalLiteral (LInt int)       = return (IntValue int)
evalLiteral (LBool bool)     = return (BoolValue bool)
evalLiteral (LChar char)     = return (CharValue char)
evalLiteral (LString string) = return (StringValue string)
evalLiteral (LVoid void)     = return VoidValue

evalAddition :: Value -> Value -> StateMonad Value
evalAddition (IntValue v1) (IntValue v2) = return $ IntValue $ v1 + v2

evalSubtraction :: Value -> Value -> StateMonad Value
evalSubtraction (IntValue v1) (IntValue v2) = return $ IntValue $ v1 - v2

evalMultiplication :: Value -> Value -> StateMonad Value
evalMultiplication (IntValue v1) (IntValue v2) = return $ IntValue $ v1 * v2

-- TODO throw if division by zero
evalDivision :: Value -> Value -> StateMonad Value
evalDivision (IntValue v1) (IntValue v2) = return $ IntValue $ v1 `div` v2

declareValue :: ValueDeclProper -> StateMonad Result
declareValue (InitializedValue identifier valueType expr) = do
    (initializationValue, _) <- evalExpr expr
    addValue identifier initializationValue
