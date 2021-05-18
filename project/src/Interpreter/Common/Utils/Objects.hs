module Interpreter.Common.Utils.Objects where

import qualified Data.Map                         as Map

import           Interpreter.Common.Types
import           Parser.Tidy.Abs

import           Interpreter.Common.Errors
import           Interpreter.Common.Utils.Builtin
import           Interpreter.Common.Utils.Types


getProperDeclaration :: ObjectDecl -> ObjectDeclProper
getProperDeclaration (ObjectDeclaration _ properDeclaration) = properDeclaration

getObjectIdentifier :: ObjectDecl -> ObjectIdent
getObjectIdentifier (ObjectDeclaration _ declProper) = objectIdentifierFromProperDeclaration declProper

objectIdentifierFromProperDeclaration :: ObjectDeclProper -> ObjectIdent
objectIdentifierFromProperDeclaration (ObjectDeclarationProper objectIdent _ _) = objectIdent

objectTypeFromDeclaration :: ObjectDecl -> ObjectType
objectTypeFromDeclaration (ObjectDeclaration _ (ObjectDeclarationProper _ objectType _)) = objectType

getObjectType :: Object -> ObjectType
getObjectType (BuiltinObject object)       = objectTypeForBuiltinObject object
getObjectType (RegularObject objectType _) = objectType

getValues :: Object -> Map.Map ObjectIdent Location
getValues (RegularObject _ (ObjectEnv values _)) = values
getValues _                                      = Map.empty

getVariables :: Object -> Map.Map ObjectIdent Location
getVariables (RegularObject _ (ObjectEnv _ variables)) = variables
getVariables _                                         = Map.empty

isInitialized :: ObjectDecl -> Bool
isInitialized (ObjectDeclaration _ (ObjectDeclarationProper _ _ (Initialized _)))  = True
isInitialized _                                                                    = False

objectToNameTypePair :: ObjectDecl -> (ObjectIdent, ObjectType)
objectToNameTypePair (ObjectDeclaration _ (ObjectDeclarationProper objectIdent objectType _)) =
    (objectIdent, objectType)

objectToNameExprPair :: ObjectDecl -> (ObjectIdent, Expr)
objectToNameExprPair (ObjectDeclaration _ (ObjectDeclarationProper objectIdent _ (Initialized expr))) =
    (objectIdent, expr)

-- TODO handle builtin objects
getAttributeLocation :: Object -> ObjectIdent -> Location
getAttributeLocation (RegularObject _ objectEnv) attributeIdent =
    if attributeIdent `Map.member` values objectEnv
    then values objectEnv Map.! attributeIdent
    else variables objectEnv Map.! attributeIdent

publicDeclarationFromProper :: ObjectDeclProper -> ObjectDecl
publicDeclarationFromProper = ObjectDeclaration MPublic
