module Interpreter.Eval.Objects where

import           Control.Monad.Reader
import qualified Data.Map                 as Map

import           Interpreter.Common.Types
import           Parser.Tidy.Abs


newSingleValueObject :: SingleValue -> Value
newSingleValueObject = SingleValueObject

newRegularObject :: ValueType -> ObjectEnv -> StateMonad Value
newRegularObject objectType objectEnv = return $ RegularObject objectType objectEnv

pass :: Value
pass = newSingleValueObject VoidValue

getObjectType :: Value -> ValueType
getObjectType (SingleValueObject object)  = valueTypeForSingleValueObject object
getObjectType (RegularObject valueType _) = valueType

valueTypeForSingleValueObject :: SingleValue -> ValueType
valueTypeForSingleValueObject (IntValue _)    = valueTypeFromClassName "Int"
valueTypeForSingleValueObject (BoolValue _)   = valueTypeFromClassName "Bool"
valueTypeForSingleValueObject (CharValue _)   = valueTypeFromClassName "Char"
valueTypeForSingleValueObject (StringValue _) = valueTypeFromClassName "String"
valueTypeForSingleValueObject VoidValue       = valueTypeFromClassName "Void"

valueTypeFromClassName :: String -> ValueType
valueTypeFromClassName name = ValueTypeClass (CIdent (UpperCaseIdent name))
