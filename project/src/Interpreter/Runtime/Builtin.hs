module Interpreter.Runtime.Builtin where

import           Control.Monad.Except
import           Control.Monad.IO.Class

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
    "__builtin_exit"         -> evaluateExitBuiltinMethod
    "__builtin_assert"       -> evaluateAssertBuiltinMethod
    "__builtin_assertEquals" -> evaluateAssertEqualsBuiltinMethod
    "__builtin_print"        -> evaluatePrintBuiltinMethod
    "__builtin_printLine"    -> evaluatePrintLineBuiltinMethod

evaluateExitBuiltinMethod :: StateMonad Result
evaluateExitBuiltinMethod = do
    argument <- getLocalObject $ objectIdentifierFromName "code"
    case argument of BuiltinObject (IntObject code) -> throwError $ UserExitException $ fromIntegral code

evaluateAssertBuiltinMethod :: StateMonad Result
evaluateAssertBuiltinMethod = do
    predicateArg <- getLocalObject $ objectIdentifierFromName "predicate"
    contextArg <- getLocalObject $ objectIdentifierFromName "context"
    case predicateArg of
        BuiltinObject (BoolObject predicate) -> case contextArg of
            BuiltinObject (StringObject context) -> unless (fromBoolean predicate)
                $ throwError $ AssertionFailedException context
    returnPass

evaluateAssertEqualsBuiltinMethod :: StateMonad Result
evaluateAssertEqualsBuiltinMethod = do
    param1 <- getLocalObject $ objectIdentifierFromName "param1"
    param2 <- getLocalObject $ objectIdentifierFromName "param2"
    contextArg <- getLocalObject $ objectIdentifierFromName "context"
    result <- objectsEqual param1 param2
    case contextArg of
        BuiltinObject (StringObject context) -> if not result
                                                then throwError $ AssertionFailedException context
                                                else returnPass

evaluatePrintBuiltinMethod :: StateMonad Result
evaluatePrintBuiltinMethod = do
    argument <- getLocalObject $ objectIdentifierFromName "value"
    objectString <- objectToString argument
    liftIO $ putStr objectString
    returnPass

evaluatePrintLineBuiltinMethod :: StateMonad Result
evaluatePrintLineBuiltinMethod = do
    argument <- getLocalObject $ objectIdentifierFromName "value"
    objectString <- objectToString argument
    liftIO $ putStrLn objectString
    returnPass
