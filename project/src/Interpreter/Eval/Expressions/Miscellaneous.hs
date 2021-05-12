module Interpreter.Eval.Expressions.Miscellaneous where

import           Interpreter.Common.Types
import           Parser.Tidy.Abs

import           Interpreter.Common.Helper.Objects


evaluateLiteral :: Literal -> StateMonad Object
evaluateLiteral (LInt int)       = return $ BuiltinObject $ IntObject int
evaluateLiteral (LBool bool)     = return $ BuiltinObject $ BoolObject bool
evaluateLiteral (LChar char)     = return $ BuiltinObject $ CharObject char
evaluateLiteral (LString string) = return $ BuiltinObject $ StringObject string
evaluateLiteral (LVoid void)     = return $ BuiltinObject VoidObject
