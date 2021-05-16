module Interpreter.Common.Utils.Objects where

import qualified Data.Map                         as Map

import           Interpreter.Common.Types
import           Parser.Tidy.Abs

import           Interpreter.Common.Errors
import           Interpreter.Common.Utils.Builtin


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

getObjectIdentifier :: ObjectDecl -> ObjectIdent
getObjectIdentifier (ObjectDeclaration _ declProper) = objectIdentifierFromProperDeclaration declProper

objectIdentifierFromProperDeclaration :: ObjectDeclProper -> ObjectIdent
objectIdentifierFromProperDeclaration (ObjectDeclarationProper objectIdent _ _) = objectIdent

objectIdentifierFromName :: String -> ObjectIdent
objectIdentifierFromName name = ObjectIdentifier (LowerCaseIdent name)

getValues :: Object -> Map.Map ObjectIdent Location
getValues (RegularObject _ (ObjectEnv values _)) = values
getValues _                                      = Map.empty

getVariables :: Object -> Map.Map ObjectIdent Location
getVariables (RegularObject _ (ObjectEnv _ variables)) = variables
getVariables _                                         = Map.empty

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

objectToMethodIdentifier :: ObjectIdent -> MethodIdent
objectToMethodIdentifier (ObjectIdentifier ident) = MethodIdentifier ident

-- TODO handle builtin objects
getAttributeLocation :: Object -> ObjectIdent -> Location
getAttributeLocation (RegularObject _ objectEnv) attributeIdent =
    if attributeIdent `Map.member` values objectEnv
    then values objectEnv Map.! attributeIdent
    else variables objectEnv Map.! attributeIdent

publicDeclarationFromProper :: ObjectDeclProper -> ObjectDecl
publicDeclarationFromProper = ObjectDeclaration MPublic

showComplexContext :: Expr -> String -> String
showComplexContext expr largerContext = showContext expr ++ "\nIn: " ++ largerContext
