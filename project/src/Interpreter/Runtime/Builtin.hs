module Interpreter.Runtime.Builtin where

import           Control.Monad.IO.Class
import           System.Exit

import           Interpreter.Common.Types
import           Parser.Tidy.Abs

import           Interpreter.Common.Utils.Types
import           Interpreter.Runtime.Environments
import           Interpreter.Runtime.Types


evaluateBuiltinMethodInEnv :: MethodIdent -> StateMonad Result
evaluateBuiltinMethodInEnv (MethodIdentifier (LowerCaseIdent methodName)) = case methodName of
    "__builtin_exit" -> evaluateBuiltinExitMethod

evaluateBuiltinExitMethod :: StateMonad Result
evaluateBuiltinExitMethod = do
    argument <- getLocalObject $ objectIdentifierFromName "code"
    liftIO $ case argument of BuiltinObject (IntObject code) -> if code == 0 then exitSuccess
                                                                else exitWith $ ExitFailure $ fromIntegral code
