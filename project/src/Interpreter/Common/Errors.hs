module Interpreter.Common.Errors where

import           System.Exit
import           System.IO

import           Parser.Tidy.Print


data RuntimeException
    = DivideByZeroException String
    | AssertionFailedException String
    | UserExitException Int
    | RuntimeException String

data CompilationError
    = NoMainActionError
    | NoSuchFileError String
    | UnexpectedTypeError String String String
    | UnexpectedReturnTypeError String String String
    | TypesDoNotMatchError String String
    | IllegalSectionError String String String
    | UninitializedError String
    | IllegalInitializationError String
    | DuplicateDeclarationError String String
    | ObjectNotInScopeError String
    | ClassNotInScopeError String
    | IllegalSideEffectsError String String
    | BodyEmptyError String
    | ConstructorArgumentListInvalidError String String String
    | MethodArgumentListInvalidError String String String
    | GenericArgumentListInvalidError String String String
    | NoSuchAttributeError String String
    | NoSuchFunctionError String String
    | NoSuchActionError String String
    | NonSingletonClassError String
    | CompilationError String

instance Show RuntimeException where
    show (DivideByZeroException context) = "DivideByZeroException: Attempted division by zero.\n" ++
        "Divisor expression evaluating to 0:\n" ++ context
    show (AssertionFailedException context) = "AssertionFailedException: Assertion failed:\n" ++ context
    show (RuntimeException message) = "RuntimeException: " ++ message

instance Show CompilationError where
    show NoMainActionError = "NoMainActionError: No singleton class with action named main exists."
    show (NoSuchFileError path) = "NoSuchFileError: Source file " ++ show path ++ " does not exist."
    show (UnexpectedTypeError expected actual context) = "UnexpectedTypeError: Types do not match." ++
        "\nExpected: " ++ expected ++ "\nActual: " ++ actual ++ "\nIn expression:\n" ++ context
    show (UnexpectedReturnTypeError expected actual context) = "UnexpectedReturnTypeError: " ++
        "Declared method return type does not match the actual one." ++ "\nExpected: " ++ expected ++
        "\nActual: " ++ actual ++ "\nIn method: " ++ context
    show (TypesDoNotMatchError context types) = "TypesDoNotMatchError: Types do not match: " ++ types ++
        "\nIn: " ++ context
    show (IllegalSectionError classType classIdent section) = "IllegalSectionError: " ++ classType ++
        " class " ++ show classIdent ++ " must not contain section " ++ section ++ "."
    show (UninitializedError name) = "UninitializedError: Object " ++ show name ++ " must be initialized."
    show (IllegalInitializationError name) = "IllegalInitializationError: Object " ++ show name ++
        " must not be initialized."
    show (DuplicateDeclarationError name context) = "DuplicateDeclarationError: Object or method with name "
        ++ show name ++ " is declared more than once in \"" ++ context ++ "\"."
    show (ObjectNotInScopeError name) = "ObjectNotInScopeError: Object " ++ show name ++
        " is not declared in this scope."
    show (ClassNotInScopeError name) = "ClassNotInScopeError: Class " ++ show name ++
        " is not declared in this scope."
    show (IllegalSideEffectsError context expr) = "IllegalSideEffectsError: Expressions inside \"" ++ context ++
        "\" must be purely functional.\nExpression with side effects:\n" ++ expr
    show (BodyEmptyError context) = "BodyEmptyError: Body of \"" ++ context ++
        "\" must contain at least one proper expression. If you only need a filler, return Pass."
    show (ConstructorArgumentListInvalidError context expected actual) =
        "ConstructorArgumentListInvalidError: Constructor called with invalid argument types.\nExpected: " ++
        expected ++ "\nActual: " ++ actual ++ "\nIn constructor call: " ++ context
    show (MethodArgumentListInvalidError context expected actual) =
        "MethodArgumentListInvalidError: Method " ++ show context ++ " called with invalid argument types." ++
        "\nExpected: " ++ expected ++ "\nActual: " ++ actual
    show (GenericArgumentListInvalidError context expected actual) =
        "GenericArgumentListInvalidError: Generic class constructor or static expression " ++ context ++
        " called with invalid generic arguments." ++ "\nExpected: " ++ expected ++ "\nActual: " ++ actual
    show (NoSuchAttributeError object attribute) = "NoSuchAttributeError: Object " ++ show object ++
        " has no attribute named " ++ attribute ++ "."
    show (NoSuchFunctionError object method) = "NoSuchFunctionError: Object " ++ show object ++
        " has no function named " ++ show method ++ "."
    show (NoSuchActionError object method) = "NoSuchActionError: Object " ++ show object ++
        " has no action named " ++ show method ++ "."
    show (NonSingletonClassError context) = "NonSingletonClassError: Non-singleton classes cannot be referenced" ++
        " from a static context.\nIn expression: " ++ context
    show (CompilationError message) = "CompilationError: " ++ message


exitWithError :: String -> IO ()
exitWithError error = hPutStrLn stderr ("Error: " ++ error) >> exitFailure

showContext :: (Print a) => a -> String
showContext = render . prt 0
