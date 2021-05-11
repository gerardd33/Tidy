module Interpreter.Eval.Actions where

import           Interpreter.Common.Types
import           Interpreter.Eval.Expressions
import           Parser.Tidy.Abs


getActionBody :: ActionDecl -> ActionBody
getActionBody (ActionDeclaration _ _ _ _ actionBody) = actionBody

-- TODO passing parameters and other context information
evalAction :: ActionDecl -> StateMonad Result
evalAction action = evalActionBody (getActionBody action)

evalActionBody :: ActionBody -> StateMonad Result
evalActionBody (ActionBodyOneLine expr)    = evalExprList [expr]
evalActionBody (ActionBodyMultiLine exprs) = evalExprList exprs
