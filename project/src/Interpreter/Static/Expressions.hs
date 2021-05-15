module Interpreter.Static.Expressions where

import           Control.Monad.Reader

import           Interpreter.Common.Types
import           Parser.Tidy.Abs


checkExpression :: Expr -> StaticCheckMonad ObjectType
checkExpression = undefined