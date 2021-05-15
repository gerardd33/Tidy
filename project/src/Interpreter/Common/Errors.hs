module Interpreter.Common.Errors where

import           System.Exit
import           System.IO

import           Parser.Tidy.Print


-- TODO later some useful info as to where they happened
data RuntimeException
    = DivideByZeroException String
    | RuntimeException String

data CompilationError
    = NoMainActionError
    | UnexpectedTypeError String String
    | ForbiddenSectionError String String String
    | UninitializedError String
    | DuplicateAttributeError String
    | CompilationError String


instance Show RuntimeException where
    show (DivideByZeroException context) = "DivideByZeroException: Attempted division by zero.\n" ++
        "Divisor expression evaluating to 0:\n" ++ context
    show (RuntimeException message) = "RuntimeException: " ++ message

instance Show CompilationError where
    show NoMainActionError = "NoMainActionError: No singleton class with action named main exists."
    show (UnexpectedTypeError expected actual) = "UnexpectedTypeError: Types do not match." ++
        "\nExpected: " ++ expected ++ "\nActual: " ++ actual
    show (ForbiddenSectionError classType classIdent section) = "ForbiddenSectionError: " ++ classType ++
        " class " ++ classIdent ++ " must not contain section " ++ section ++ "."
    show (UninitializedError name) = "UninitializedError: Object " ++ name ++ " must be initialized."
    show (DuplicateAttributeError name) = "DuplicateAttributeError: Attribute " ++ name ++ " already exists."
    show (CompilationError message) = "CompilationError: " ++ message


exitWithError :: String -> IO ()
exitWithError error = hPutStrLn stderr ("Error: " ++ error) >> exitFailure

showContext :: (Print a) => a -> String
showContext = render . prt 0
