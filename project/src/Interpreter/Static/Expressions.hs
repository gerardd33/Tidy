module Interpreter.Static.Expressions where

import           Control.Monad.Reader

import           Interpreter.Common.Types
import           Parser.Tidy.Abs

import           Interpreter.Static.Types


checkExpression :: Expr -> StaticCheckMonad ObjectType
checkExpression expr = returnVoid
