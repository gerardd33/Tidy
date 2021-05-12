module Interpreter.Eval.Expressions.Evaluate where

import           Interpreter.Common.Types
import           Parser.Tidy.Abs

import           Interpreter.Eval.Expressions.Miscellaneous
import           Interpreter.Eval.Utils


evaluateExpressionList :: [Expr] -> StateMonad Result
evaluateExpressionList [expr] = evaluateExpression expr
evaluateExpressionList (expr:exprs) = do
    (_, env) <- evaluateExpression expr
    local (const env) (evaluateExpressionList exprs)

evaluateExpression :: Expr -> StateMonad Result
evaluateExpression (ELiteral literal) = returnPure $ evalLiteral literal
