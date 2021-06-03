module Interpreter.Runtime.Builtin where

import           Interpreter.Common.Types
import           Parser.Tidy.Abs

import           Interpreter.Runtime.Types


evaluateBuiltinMethodInEnv :: MethodIdent -> StateMonad Result
evaluateBuiltinMethodInEnv (MethodIdentifier (LowerCaseIdent methodName)) = case methodName of
    "__builtin_twice" -> evaluateBuiltinTwiceMethod

evaluateBuiltinTwiceMethod :: StateMonad Result
evaluateBuiltinTwiceMethod = liftPure $ return $ BuiltinObject $ IntObject 3
