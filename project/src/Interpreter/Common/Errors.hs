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
    | UnexpectedTypeError String String String
    | UnexpectedReturnTypeError String String String
    | IllegalSectionError String String String
    | UninitializedError String
    | IllegalInitializationError String
    | DuplicateDeclarationError String String
    | ObjectNotInScopeError String
    | IllegalSideEffectsError String String
    | CompilationError String

instance Show RuntimeException where
    show (DivideByZeroException context) = "DivideByZeroException: Attempted division by zero.\n" ++
        "Divisor expression evaluating to 0:\n" ++ context
    show (RuntimeException message) = "RuntimeException: " ++ message

instance Show CompilationError where
    show NoMainActionError = "NoMainActionError: No singleton class with action named main exists."
    show (UnexpectedTypeError expected actual context) = "UnexpectedTypeError: Types do not match." ++
        "\nExpected: " ++ expected ++ "\nActual: " ++ actual ++ "\nIn expression:\n" ++ context
    show (UnexpectedReturnTypeError expected actual context) = "UnexpectedReturnTypeError: " ++
        "Declared method return type does not match the actual one." ++ "\nExpected: " ++ expected ++
        "\nActual: " ++ actual ++ "\nIn method: " ++ context
    show (IllegalSectionError classType classIdent section) = "IllegalSectionError: " ++ classType ++
        " class " ++ show classIdent ++ " must not contain section " ++ section ++ "."
    show (UninitializedError name) = "UninitializedError: Object " ++ show name ++ " must be initialized."
    show (IllegalInitializationError name) = "IllegalInitializationError: Object " ++ show name ++
        " must not be initialized."
    show (DuplicateDeclarationError name context) = "DuplicateDeclarationError: Object or method with name "
        ++ show name ++ " is declared more than once in " ++ show context ++ "."
    show (ObjectNotInScopeError name) = "ObjectNotInScopeError: Object " ++ show name ++
        " is not declared in this scope."
    show (IllegalSideEffectsError context expr) = "IllegalSideEffectsError: Expressions within " ++ show context ++
        " must be purely functional.\nExpression with side effects:\n" ++ expr
    show (CompilationError message) = "CompilationError: " ++ message


exitWithError :: String -> IO ()
exitWithError error = hPutStrLn stderr ("Error: " ++ error) >> exitFailure

showContext :: (Print a) => a -> String
showContext = render . prt 0
