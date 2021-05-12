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

newBuiltinObjectObject :: BuiltinObject -> Object
newBuiltinObjectObject = BuiltinObjectObject

getObjectType :: Object -> ObjectType
getObjectType (BuiltinObjectObject object)  = valueTypeForBuiltinObjectObject object
getObjectType (RegularObject valueType _) = valueType

valueTypeForBuiltinObjectObject :: BuiltinObject -> ObjectType
valueTypeForBuiltinObjectObject (IntObject _)    = valueTypeFromClassName "Int"
valueTypeForBuiltinObjectObject (BoolObject _)   = valueTypeFromClassName "Bool"
valueTypeForBuiltinObjectObject (CharObject _)   = valueTypeFromClassName "Char"
valueTypeForBuiltinObjectObject (StringObject _) = valueTypeFromClassName "String"
valueTypeForBuiltinObjectObject VoidObject       = valueTypeFromClassName "Void"

valueTypeFromClassName :: String -> ObjectType
valueTypeFromClassName name = ObjectTypeClass (ClassIdentifier (UpperCaseIdent name)) GenericParameterAbsent

pass :: Object
pass = newBuiltinObjectObject VoidObject
