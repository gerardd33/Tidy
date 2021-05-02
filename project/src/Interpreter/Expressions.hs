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
evalLiteral (LChar char) = return (CharValue char)
evalLiteral (LString string) = return (StringValue string)
evalLiteral (LVoid void) = return VoidValue

declareValue :: ValueDeclProper -> StateMonad Value
declareValue (InitialisedValue identifier valueType expr) = do
    initialisationValue <- evalExpr expr
    setValue identifier initialisationValue
