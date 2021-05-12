module Interpreter.Common.Helper.Objects where

import           Interpreter.Common.Types
import           Parser.Tidy.Abs









isInitialized :: ObjectDecl -> Bool
isInitialized (ObjectDeclaration _ (ObjectDeclarationProper _ _ (Initialized _)))  = True
isInitialized _                                                                    = False

toNameTypePair :: ObjectDecl -> (ObjectIdent, ObjectType)
toNameTypePair (ObjectDeclaration _ (ObjectDeclarationProper objectIdentifier objectType _))
    = (objectIdentifier, objectType)

getObjectName :: ObjectDecl -> ObjectIdent
getObjectName (ObjectDeclaration _ (ObjectDeclarationProper objectIdentifier _ _)) = objectIdentifier

toNameExprPair :: ObjectDecl -> (ObjectIdent, Expr)
toNameExprPair (ObjectDeclaration _ (ObjectDeclarationProper objectIdentifier _ (Initialized expression))) =
    (objectIdentifier, expression)

getProperObjectDecl :: ObjectDecl -> ObjectDeclProper
getProperObjectDecl (ObjectDeclaration _ properDeclaration) = properDeclaration

newBuiltinObject :: BuiltinObject -> Object
newBuiltinObject = BuiltinObject

getObjectType :: Object -> ObjectType
getObjectType (BuiltinObject object)      = valueTypeForBuiltinObject object
getObjectType (RegularObject valueType _) = valueType

valueTypeForBuiltinObject :: BuiltinObject -> ObjectType
valueTypeForBuiltinObject (IntObject _)    = valueTypeFromClassName "Int"
valueTypeForBuiltinObject (BoolObject _)   = valueTypeFromClassName "Bool"
valueTypeForBuiltinObject (CharObject _)   = valueTypeFromClassName "Char"
valueTypeForBuiltinObject (StringObject _) = valueTypeFromClassName "String"
valueTypeForBuiltinObject VoidObject       = valueTypeFromClassName "Void"

valueTypeFromClassName :: String -> ObjectType
valueTypeFromClassName name = ObjectTypeClass (ClassIdentifier (UpperCaseIdent name)) GenericParameterAbsent

pass :: Object
pass = newBuiltinObject VoidObject
