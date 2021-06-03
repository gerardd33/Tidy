module Interpreter.Runtime.Builtin where

import           Interpreter.Common.Types
import           Parser.Tidy.Abs

import           Interpreter.Common.Utils.Types
import           Interpreter.Runtime.Environments
import           Interpreter.Runtime.Types


evaluateBuiltinMethodInEnv :: MethodIdent -> StateMonad Result
evaluateBuiltinMethodInEnv (MethodIdentifier (LowerCaseIdent methodName)) = case methodName of
    "__builtin_twice" -> evaluateBuiltinTwiceMethod

evaluateBuiltinTwiceMethod :: StateMonad Result
evaluateBuiltinTwiceMethod = do
    argument <- getLocalObject $ objectIdentifierFromName "x"
    case argument of BuiltinObject (IntObject value) -> liftPure $ return $ BuiltinObject $ IntObject $ 2 * value
