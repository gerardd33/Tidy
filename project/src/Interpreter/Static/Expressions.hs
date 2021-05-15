module Interpreter.Static.Expressions where

import           Control.Monad.Reader

import           Interpreter.Common.Types
import           Parser.Tidy.Abs

import           Interpreter.Static.Environments
import           Interpreter.Static.Operators
import           Interpreter.Static.Types


checkExpression :: Expr -> StaticCheckMonad ObjectType
checkExpression (ELiteral literal)       = checkLiteral literal
checkExpression (ELocalValue identifier) = checkLocalObject identifier
