module Interpreter.Common.Utils.Builtin where

import           Interpreter.Common.Types
import           Parser.Tidy.Abs


pass :: Object
pass = BuiltinObject VoidObject

objectTypeFromClassName :: String -> ObjectType
objectTypeFromClassName name = ObjectTypeClass (ClassIdentifier (UpperCaseIdent name)) GenericParameterAbsent

objectTypeForBuiltinObject :: BuiltinObject -> ObjectType
objectTypeForBuiltinObject (IntObject _)    = intType
objectTypeForBuiltinObject (BoolObject _)   = objectTypeFromClassName "Bool"
objectTypeForBuiltinObject (CharObject _)   = objectTypeFromClassName "Char"
objectTypeForBuiltinObject (StringObject _) = objectTypeFromClassName "String"
objectTypeForBuiltinObject VoidObject       = objectTypeFromClassName "Void"

intType :: ObjectType
intType = objectTypeFromClassName "Int"

boolType :: ObjectType
boolType = objectTypeFromClassName "Bool"

charType :: ObjectType
charType = objectTypeFromClassName "Char"

stringType :: ObjectType
stringType = objectTypeFromClassName "String"

voidType :: ObjectType
voidType = objectTypeFromClassName "Void"
