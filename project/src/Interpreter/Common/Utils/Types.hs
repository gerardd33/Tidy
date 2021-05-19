module Interpreter.Common.Utils.Types where

import           Interpreter.Common.Types
import           Parser.Tidy.Abs


toBoolean :: Bool -> Boolean
toBoolean True  = BTrue
toBoolean False = BFalse

fromBoolean :: Boolean -> Bool
fromBoolean BTrue  = True
fromBoolean BFalse = False

isTrue :: Object -> Bool
isTrue (BuiltinObject (BoolObject BTrue))  = True
isTrue (BuiltinObject (BoolObject BFalse)) = False

classIdentifierFromName :: String -> ClassIdent
classIdentifierFromName name = ClassIdentifier (UpperCaseIdent name)

objectIdentifierFromName :: String -> ObjectIdent
objectIdentifierFromName name = ObjectIdentifier (LowerCaseIdent name)

objectTypeFromClassName :: String -> ObjectType
objectTypeFromClassName name = ObjectTypeClass (ClassIdentifier (UpperCaseIdent name)) GenericParameterAbsent

objectToMethodIdentifier :: ObjectIdent -> MethodIdent
objectToMethodIdentifier (ObjectIdentifier ident) = MethodIdentifier ident

methodToObjectIdentifier :: MethodIdent -> ObjectIdent
methodToObjectIdentifier (MethodIdentifier ident) = ObjectIdentifier ident
