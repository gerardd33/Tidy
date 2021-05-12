module Interpreter.Eval.Expressions.Miscellaneous where

import           Interpreter.Common.Types
import           Parser.Tidy.Abs

import           Interpreter.Common.Helper.Objects


evaluateLiteral :: Literal -> StateMonad Object
evaluateLiteral (LInt int)       = return $ newBuiltinObject $ IntObject int
evaluateLiteral (LBool bool)     = return $ newBuiltinObject $ BoolObject bool
evaluateLiteral (LChar char)     = return $ newBuiltinObject $ CharObject char
evaluateLiteral (LString string) = return $ newBuiltinObject $ StringObject string
evaluateLiteral (LVoid void)     = return $ newBuiltinObject VoidObject
