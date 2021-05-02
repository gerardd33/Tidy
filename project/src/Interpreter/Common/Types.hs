module Interpreter.Common.Types where

import           Parser.Tidy.Abs


-- TODO other types, especially proper objects
data Value = IntValue Integer
    | BoolValue Boolean
    | CharValue Char
    | StringValue String
    | VoidValue
    deriving (Eq, Show)

