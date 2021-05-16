module Interpreter.Static.Expressions where

import           Control.Monad.Except
import           Control.Monad.Reader

import           Interpreter.Common.Types
import           Parser.Tidy.Abs

import           Interpreter.Common.Errors
import           Interpreter.Common.Utils.Builtin
import           Interpreter.Common.Utils.Expressions
import           Interpreter.Static.Environments
import           Interpreter.Static.Operators
import           Interpreter.Static.Types


checkExpression :: String -> Expr -> StaticCheckMonad StaticResult
checkExpression _ (ELiteral literal)       = liftPureStatic $ checkLiteral literal
checkExpression _ (ELocalValue identifier) = liftPureStatic $ checkLocalObject identifier
-- TODO finish implementing
checkExpression _ (ELocalDeclaration _) = liftPureStatic returnVoid
checkExpression _ _ = liftPureStatic $ return intType

assertPureExpression :: String -> Expr -> StaticCheckMonad ObjectType
assertPureExpression context expr = do
    unless (isExpressionPure expr) $ throwError $ IllegalSideEffectsError context (showContext expr)
    returnVoid
