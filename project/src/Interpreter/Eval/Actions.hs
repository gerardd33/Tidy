module Interpreter.Eval.Actions where

import           Interpreter.Common.Types
import           Interpreter.Eval.Expressions
import           Parser.Tidy.Abs


getActionBody :: ActionDecl -> ActionBody
getActionBody (OverrideActionDecl _ _ actionBody) = actionBody
getActionBody (PublicActionDecl _ _ actionBody)   = actionBody
getActionBody (PrivateActionDecl _ _ actionBody)  = actionBody

-- TODO passing parameters and other context information
evalAction :: ActionDecl -> StateMonad Result
evalAction action = evalActionBody (getActionBody action)

evalActionBody :: ActionBody -> StateMonad Result
evalActionBody (ActionBodyOneLine expr)    = evalExpressionList [expr]
evalActionBody (ActionBodyMultiLine exprs) = evalExpressionList exprs
