module Interpreter.Expressions where

import           Control.Monad.Reader
import           Data.Maybe

import           Interpreter.State
import           Parser.Tidy.Abs


evalExpressionList :: [Expr] -> StateMonad Value
evalExpressionList exprs = last <$> mapM evalExpr exprs

evalExpr :: Expr -> StateMonad Value
evalExpr (ELiteral literal) = evalLiteral literal
evalExpr (ELocalValue identifier) = getValue identifier
evalExpr (ELocalValueDecl (LocalVDecl (PublicValueDecl decl))) = declareValue decl

evalExpr (EAdd expr1 expr2) = do
    v1 <- evalExpr expr1
    v2 <- evalExpr expr2
    evalAddition v1 v2

evalExpr (ESubtract expr1 expr2) = do
    v1 <- evalExpr expr1
    v2 <- evalExpr expr2
    evalSubtraction v1 v2

evalExpr (EMultiply expr1 expr2) = do
    v1 <- evalExpr expr1
    v2 <- evalExpr expr2
    evalMultiplication v1 v2

evalExpr (EDivide expr1 expr2) = do
    v1 <- evalExpr expr1
    v2 <- evalExpr expr2
    evalDivision v1 v2

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

declareValue :: ValueDeclProper -> StateMonad Value
declareValue (InitialisedValue identifier valueType expr) = do
    initialisationValue <- evalExpr expr
    setValue identifier initialisationValue
