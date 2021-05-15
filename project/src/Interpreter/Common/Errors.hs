module Interpreter.Common.Errors where

import           System.Exit
import           System.IO


-- TODO other exceptions
-- TODO better printing (custom show instance)
data RuntimeException
    = DivideByZeroException
    | RuntimeException String

data CompilationError
    = NoMainMethodError
    | UnexpectedTypeError String String
    | CompilationError String


instance Show RuntimeException where
    show DivideByZeroException = "DivideByZeroException: Attempted division by zero."
    show (RuntimeException message) = "RuntimeException: " ++ message

instance Show CompilationError where
    show NoMainMethodError = "NoMainMethodError: No class contains a method named main."
    show (UnexpectedTypeError expected actual) = "UnexpectedTypeError: Types do not match." ++
        "\nExpected: " ++ expected ++ "\nActual: " ++ actual
    show (CompilationError message) = "CompilationError: " ++ message


exitWithError :: String -> IO ()
exitWithError error = hPutStrLn stderr ("Error: " ++ error) >> exitFailure
