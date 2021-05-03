module Interpreter.Eval.Functions where

import           Interpreter.Common.Types
import           Parser.Tidy.Abs


argsToExprList :: ArgumentList -> [Expr]
argsToExprList ArgListAbsent         = []
argsToExprList (ArgListPresent args) = map argToExpr args

argToExpr :: FunctionArgument -> Expr
argToExpr (FunctionArg expr) = expr
