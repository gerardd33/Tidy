module Interpreter.Common.Utils.Objects where

import qualified Data.Map                 as Map

import           Interpreter.Common.Types
import           Parser.Tidy.Abs


pass :: Object
pass = BuiltinObject VoidObject

localReferenceType :: ObjectType
localReferenceType = objectTypeFromClassName "__local"

localReferenceIdentifier :: ObjectIdent
localReferenceIdentifier = objectIdentifierFromName "local"

thisReferenceIdentifier :: ObjectIdent
thisReferenceIdentifier = objectIdentifierFromName "this"

getProperDeclaration :: ObjectDecl -> ObjectDeclProper
getProperDeclaration (ObjectDeclaration _ properDeclaration) = properDeclaration

getObjectType :: Object -> ObjectType
getObjectType (BuiltinObject object)       = objectTypeForBuiltinObject object
getObjectType (RegularObject objectType _) = objectType

getValues :: Object -> Map.Map ObjectIdent Location
getValues (RegularObject _ (ObjectEnv values _)) = values
getValues _                                      = Map.empty

getVariables :: Object -> Map.Map ObjectIdent Location
getVariables (RegularObject _ (ObjectEnv _ variables)) = variables
getVariables _                                         = Map.empty

objectTypeForBuiltinObject :: BuiltinObject -> ObjectType
objectTypeForBuiltinObject (IntObject _)    = objectTypeFromClassName "Int"
objectTypeForBuiltinObject (BoolObject _)   = objectTypeFromClassName "Bool"
objectTypeForBuiltinObject (CharObject _)   = objectTypeFromClassName "Char"
objectTypeForBuiltinObject (StringObject _) = objectTypeFromClassName "String"
objectTypeForBuiltinObject VoidObject       = objectTypeFromClassName "Void"

voidType :: ObjectType
voidType = objectTypeForBuiltinObject VoidObject

objectNameFromDeclaration :: ObjectDecl -> ObjectIdent
objectNameFromDeclaration (ObjectDeclaration _ (ObjectDeclarationProper objectIdent _ _)) = objectIdent

objectTypeFromClassName :: String -> ObjectType
objectTypeFromClassName name = ObjectTypeClass (ClassIdentifier (UpperCaseIdent name)) GenericParameterAbsent

objectIdentifierFromName :: String -> ObjectIdent
objectIdentifierFromName name = ObjectIdentifier (LowerCaseIdent name)

isInitialized :: ObjectDecl -> Bool
isInitialized (ObjectDeclaration _ (ObjectDeclarationProper _ _ (Initialized _)))  = True
isInitialized _                                                                    = False

toNameTypePair :: ObjectDecl -> (ObjectIdent, ObjectType)
toNameTypePair (ObjectDeclaration _ (ObjectDeclarationProper objectIdent objectType _)) =
    (objectIdent, objectType)

toNameExprPair :: ObjectDecl -> (ObjectIdent, Expr)
toNameExprPair (ObjectDeclaration _ (ObjectDeclarationProper objectIdent _ (Initialized expr))) =
    (objectIdent, expr)

methodToObjectIdentifier :: MethodIdent -> ObjectIdent
methodToObjectIdentifier (MethodIdentifier ident) = ObjectIdentifier ident

-- TODO handle builtin objects
getAttributeLocation :: Object -> ObjectIdent -> Location
getAttributeLocation (RegularObject _ objectEnv) attributeIdent =
    if attributeIdent `Map.member` values objectEnv
    then values objectEnv Map.! attributeIdent
    else variables objectEnv Map.! attributeIdent
