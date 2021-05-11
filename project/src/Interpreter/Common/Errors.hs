module Interpreter.Common.Errors where


-- TODO other exceptions
-- TODO rename OtherException to RuntimeException if possible
-- TODO better printing (custom show instance)
data RuntimeException = DivideByZeroException
    | OtherException
    deriving (Show)

data CompilationError = NoMainMethodError
    | OtherError
    deriving (Show)
