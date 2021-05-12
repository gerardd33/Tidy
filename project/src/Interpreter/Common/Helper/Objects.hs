module Interpreter.Common.Helper.Objects where

import           Interpreter.Common.Types
import           Parser.Tidy.Abs


getProperDeclaration :: ObjectDecl -> ObjectDeclProper
getProperDeclaration (ObjectDeclaration _ properDeclaration) = properDeclaration

getLocalObjectType :: Object -> ObjectType
getLocalObjectType (BuiltinObject object)       = objectTypeForBuiltinObject object
getLocalObjectType (RegularObject objectType _) = objectType

objectTypeForBuiltinObject :: BuiltinObject -> ObjectType
objectTypeForBuiltinObject (IntObject _)    = objectTypeFromClassName "Int"
objectTypeForBuiltinObject (BoolObject _)   = objectTypeFromClassName "Bool"
objectTypeForBuiltinObject (CharObject _)   = objectTypeFromClassName "Char"
objectTypeForBuiltinObject (StringObject _) = objectTypeFromClassName "String"
objectTypeForBuiltinObject VoidObject       = objectTypeFromClassName "Void"

objectTypeFromClassName :: String -> ObjectType
objectTypeFromClassName name = ObjectTypeClass (ClassIdentifier (UpperCaseIdent name)) GenericParameterAbsent




isInitialized :: ObjectDecl -> Bool
isInitialized (ObjectDeclaration _ (ObjectDeclarationProper _ _ (Initialized _)))  = True
isInitialized _                                                                    = False

toNameTypePair :: ObjectDecl -> (ObjectIdent, ObjectType)
toNameTypePair (ObjectDeclaration _ (ObjectDeclarationProper objectIdent objectType _))
    = (objectIdent, objectType)

getLocalValueName :: ObjectDecl -> ObjectIdent
getLocalValueName (ObjectDeclaration _ (ObjectDeclarationProper objectIdent _ _)) = objectIdent

toNameExprPair :: ObjectDecl -> (ObjectIdent, Expr)
toNameExprPair (ObjectDeclaration _ (ObjectDeclarationProper objectIdent _ (Initialized expr))) =
    (objectIdent, expr)



