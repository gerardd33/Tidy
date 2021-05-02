module Interpreter.Common.Types where

import qualified Data.Map        as Map

import           Parser.Tidy.Abs


-- TODO other types, especially proper objects
data Value = IntValue Integer
    | BoolValue Boolean
    | CharValue Char
    | StringValue String
    | VoidValue
    deriving (Eq, Show)

data Object = Object ValueType ObjectEnv

data ObjectEnv = ObjectEnv { values :: ValueEnv, variables :: ValueEnv }
type ValueEnv = Map.Map ValueIdent Object
