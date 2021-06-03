module Interpreter.Runtime.Builtin where

import           Control.Monad.Except
import           Control.Monad.IO.Class
import           System.Exit

import           Interpreter.Common.Types
import           Parser.Tidy.Abs

import           Interpreter.Common.Errors
import           Interpreter.Common.Utils.Types
import           Interpreter.Runtime.Environments
import           Interpreter.Runtime.Types


evaluateBuiltinMethodCall :: MethodIdent -> StateMonad Result
evaluateBuiltinMethodCall (MethodIdentifier (LowerCaseIdent methodName)) = case methodName of
    "__builtin_exit"   -> evaluateBuiltinExitMethod
    "__builtin_assert" -> evaluateBuiltinAssertMethod

evaluateBuiltinExitMethod :: StateMonad Result
evaluateBuiltinExitMethod = do
    argument <- getLocalObject $ objectIdentifierFromName "code"
    liftIO $ case argument of BuiltinObject (IntObject code) -> if code == 0 then exitSuccess
                                                                else exitWith $ ExitFailure $ fromIntegral code

evaluateBuiltinAssertMethod :: StateMonad Result
evaluateBuiltinAssertMethod = do
    predicateArg <- getLocalObject $ objectIdentifierFromName "predicate"
    contextArg <- getLocalObject $ objectIdentifierFromName "context"
    case predicateArg of
        BuiltinObject (BoolObject predicate) -> case contextArg of
            BuiltinObject (StringObject context) -> unless (fromBoolean predicate)
                $ throwError $ AssertionFailedException context
    returnPass
