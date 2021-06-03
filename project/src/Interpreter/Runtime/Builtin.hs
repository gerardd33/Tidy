module Interpreter.Runtime.Builtin where

import           Control.Monad.Except
import           Control.Monad.IO.Class
import           System.Exit

import           Interpreter.Common.Types
import           Parser.Tidy.Abs

import           Interpreter.Common.Errors
import           Interpreter.Common.Utils.Objects
import           Interpreter.Common.Utils.Types
import           Interpreter.Runtime.Environments
import           Interpreter.Runtime.Operators
import           Interpreter.Runtime.Types


evaluateBuiltinMethodCall :: MethodIdent -> StateMonad Result
evaluateBuiltinMethodCall (MethodIdentifier (LowerCaseIdent methodName)) = case methodName of
    "__builtin_exit"         -> evaluateBuiltinExitMethod
    "__builtin_assert"       -> evaluateBuiltinAssertMethod
    "__builtin_assertEquals" -> evaluateBuiltinAssertEqualsMethod
    "__builtin_print"        -> evaluateBuiltinPrintMethod
    "__builtin_printLine"    -> evaluateBuiltinPrintLineMethod

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

evaluateBuiltinAssertEqualsMethod :: StateMonad Result
evaluateBuiltinAssertEqualsMethod = do
    param1 <- getLocalObject $ objectIdentifierFromName "param1"
    param2 <- getLocalObject $ objectIdentifierFromName "param2"
    contextArg <- getLocalObject $ objectIdentifierFromName "context"
    result <- objectsEqual param1 param2
    case contextArg of
        BuiltinObject (StringObject context) -> if not result
                                                then throwError $ AssertionFailedException context
                                                else returnPass

evaluateBuiltinPrintMethod :: StateMonad Result
evaluateBuiltinPrintMethod = do
    argument <- getLocalObject $ objectIdentifierFromName "value"
    objectString <- objectToString argument
    liftIO $ putStr objectString
    returnPass

evaluateBuiltinPrintLineMethod :: StateMonad Result
evaluateBuiltinPrintLineMethod = do
    argument <- getLocalObject $ objectIdentifierFromName "value"
    objectString <- objectToString argument
    liftIO $ putStrLn objectString
    returnPass
