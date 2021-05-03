module Interpreter.Common.Types where

import qualified Data.Map                 as Map

import           Interpreter.Common.Utils
import           Parser.Tidy.Abs

-- TODO better printing (custom show instance)
data Value = RegularObject ValueType ObjectEnv | SingleValueObject SingleValue
    deriving (Show)

data ObjectEnv = ObjectEnv { values :: ValueEnv, variables :: ValueEnv }
    deriving (Show)

type ValueEnv = Map.Map ValueIdent Value
type Arguments = ValueEnv

data SingleValue = IntValue Integer
    | BoolValue Boolean
    | CharValue Char
    | StringValue [Char]
    | VoidValue
    deriving (Eq, Show)


newSingleValueObject :: SingleValue -> Value
newSingleValueObject = SingleValueObject

newRegularObject :: ValueType -> Arguments -> Value
newRegularObject valueType args = RegularObject valueType (objectEnvFromArgs args)

pass :: Value
pass = newSingleValueObject VoidValue

objectEnvFromArgs :: Arguments -> ObjectEnv
objectEnvFromArgs = undefined
