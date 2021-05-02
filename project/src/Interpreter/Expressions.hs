module Interpreter.Expressions where

import           Control.Monad.Reader
import           Data.Maybe

import           Interpreter.State
import           Parser.Tidy.Abs


evalExpressionList :: [Expr] -> StateMonad Result
evalExpressionList [expr] = evalExpr expr
evalExpressionList (expr:exprs) = do
    (_, localEnv) <- evalExpr expr
    local (const localEnv) (evalExpressionList exprs)

evalExpr :: Expr -> StateMonad Result
evalExpr (ELiteral literal) = do
    result <- evalLiteral literal
    localEnv <- ask
    return (Just result, localEnv)

evalExpr (ELocalValue identifier) = do
    localEnv <- ask
    value <- getValue identifier
    return (Just value, localEnv)

evalExpr (ELocalValueDecl (LocalVDecl (PublicValueDecl decl))) =
    declareValue decl

evalLiteral :: Literal -> StateMonad Value
evalLiteral (LInt int)   = return (IntValue int)
evalLiteral (LBool bool) = return (BoolValue bool)
evalLiteral (LVoid void) = return VoidValue
-- TODO char, string

declareValue :: ValueDeclProper -> StateMonad Result
declareValue (InitialisedValue identifier valueType expr) = do
    (initializationValue, _) <- evalExpr expr
    addValue identifier $ fromJust initializationValue
-- TODO change from int to any type
