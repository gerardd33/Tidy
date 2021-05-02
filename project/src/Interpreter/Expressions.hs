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

evalLiteral :: Literal -> StateMonad Value
evalLiteral (LInt int)   = return (IntValue int)
evalLiteral (LBool bool) = return (BoolValue bool)
evalLiteral (LVoid void) = return VoidValue
-- TODO char, string

declareValue :: ValueDeclProper -> StateMonad Value
declareValue (InitialisedValue identifier valueType expr) = do
    initialisationValue <- evalExpr expr
    setValue identifier initialisationValue
