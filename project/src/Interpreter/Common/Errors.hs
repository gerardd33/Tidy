module Interpreter.Common.Errors where

import           System.Exit
import           System.IO


-- TODO later some useful info as to where they happened
data RuntimeException
    = DivideByZeroException
    | RuntimeException String

data CompilationError
    = NoMainActionError
    | UnexpectedTypeError String String
    | ForbiddenSectionError String String
    | UninitializedValueError String
    | CompilationError String


instance Show RuntimeException where
    show DivideByZeroException = "DivideByZeroException: Attempted division by zero."
    show (RuntimeException message) = "RuntimeException: " ++ message

instance Show CompilationError where
    show NoMainActionError = "NoMainActionError: No singleton class with action named main exists."
    show (UnexpectedTypeError expected actual) = "UnexpectedTypeError: Types do not match." ++
        "\nExpected: " ++ expected ++ "\nActual: " ++ actual
    show (ForbiddenSectionError classType section) = "ForbiddenSectionError: " ++ classType ++
        " class must not contain section " ++ section ++ "."
    show (CompilationError message) = "CompilationError: " ++ message
    show (UninitializedValueError name) = "UninitializedValueError: Value " ++ name ++ " must be initialized."


exitWithError :: String -> IO ()
exitWithError error = hPutStrLn stderr ("Error: " ++ error) >> exitFailure
