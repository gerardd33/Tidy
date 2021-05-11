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

newSingleValueObject :: SingleValue -> Value
newSingleValueObject = SingleValueObject

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
