module Interpreter.Common.Types where

import qualified Data.Map                 as Map

import           Interpreter.Common.Utils
import           Parser.Tidy.Abs

-- TODO better printing (custom show instance)
data Value = Object ObjectType ObjectEnv
    deriving (Show)
data ObjectType = UserObject ValueType | BuiltinObject ValueType
    deriving (Show)
data ObjectEnv = ObjectEnv { values :: ValueEnv, variables :: ValueEnv }
    deriving (Show)
type ValueEnv = Map.Map ValueIdent Value

-- TODO add constructor arguments
newObject :: ObjectType -> Value
newObject (BuiltinObject objectType) = newBuiltinObject objectType
newObject (UserObject objectType)    = undefined
-- TODO case for UserObject

emptyObjectEnv :: ObjectEnv
emptyObjectEnv = ObjectEnv Map.empty Map.empty

newBuiltinObject :: ValueType -> Value
newBuiltinObject (ValueTypeClass (CIdent (UpperCaseIdent "Int"))) =
    Object intObject emptyObjectEnv
-- TODO other cases

-- TODO extract to a built-ins module
intObject :: ObjectType
intObject = BuiltinObject $ ValueTypeClass (classIdent "Int")

boolObject :: ObjectType
boolObject = BuiltinObject $ ValueTypeClass (classIdent "Bool")

charObject :: ObjectType
charObject = BuiltinObject $ ValueTypeClass (classIdent "Char")

stringObject :: ObjectType
stringObject = BuiltinObject $ ValueTypeClass (classIdent "String")

voidObject :: ObjectType
voidObject = BuiltinObject $ ValueTypeClass (classIdent "Void")

pass :: Value
pass = newObject voidObject
