module Interpreter.Eval.Methods where

import           Interpreter.Common.Types
import           Parser.Tidy.Abs

import           Interpreter.Common.Helper.Methods
import           Interpreter.Eval.Expressions.Evaluate


-- TODO passing parameters and other context information
evaluateAction :: ActionDecl -> StateMonad Result
evaluateAction = evaluateActionBody . getActionBody

evaluateActionBody :: ActionBody -> StateMonad Result
evaluateActionBody (ActionBodyOneLine expr)    = evaluateExpressionList [expr]
evaluateActionBody (ActionBodyMultiLine exprs) = evaluateExpressionList exprs
