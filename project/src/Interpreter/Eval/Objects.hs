module Interpreter.Eval.Objects where

import           Control.Monad.Reader
import qualified Data.Map                 as Map

import           Interpreter.Common.Types
import           Parser.Tidy.Abs


newSingleValueObject :: SingleValue -> Value
newSingleValueObject = SingleValueObject

newRegularObject :: ObjectType -> ObjectEnv -> StateMonad Value
newRegularObject objectType objectEnv = return $ RegularObject objectType objectEnv

pass :: Value
pass = newSingleValueObject VoidValue

getObjectType :: Value -> ObjectType
getObjectType (SingleValueObject object)  = valueTypeForSingleValueObject object
getObjectType (RegularObject valueType _) = valueType

valueTypeForSingleValueObject :: SingleValue -> ObjectType
valueTypeForSingleValueObject (IntValue _)    = valueTypeFromClassName "Int"
valueTypeForSingleValueObject (BoolValue _)   = valueTypeFromClassName "Bool"
valueTypeForSingleValueObject (CharValue _)   = valueTypeFromClassName "Char"
valueTypeForSingleValueObject (StringValue _) = valueTypeFromClassName "String"
valueTypeForSingleValueObject VoidValue       = valueTypeFromClassName "Void"

valueTypeFromClassName :: String -> ObjectType
valueTypeFromClassName name = ObjectTypeClass (ClassIdentifier (UpperCaseIdent name)) GenericParameterAbsent
