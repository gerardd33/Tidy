module Interpreter.Common.Utils.Classes where

import qualified Data.List                        as List
import qualified Data.Map                         as Map
import           Data.Maybe

import           Interpreter.Common.Types
import           Parser.Tidy.Abs

import           Interpreter.Common.Utils.Methods
import           Interpreter.Common.Utils.Objects


getClassIdentifier :: ClassDecl -> ClassIdent
getClassIdentifier (ClassDeclaration _ _ classIdent _ _) = classIdent

getClassType :: ClassDecl -> ClassTypeModifier
getClassType (ClassDeclaration _ classType _ _ _) = classType

getValueDeclarations :: ClassDecl -> [ObjectDecl]
getValueDeclarations (ClassDeclaration _ _ _ _ (ClassBodyFilled (ValuesPresent valueDeclarations) _ _ _)) =
    valueDeclarations
getValueDeclarations _ = []

getVariableDeclarations :: ClassDecl -> [ObjectDecl]
getVariableDeclarations (ClassDeclaration _ _ _ _ (ClassBodyFilled _ (VariablesPresent variableDeclarations) _ _)) =
    variableDeclarations
getVariableDeclarations _ = []

getFunctionDeclarations :: ClassDecl -> [FunctionDecl]
getFunctionDeclarations (ClassDeclaration _ _ _ _ (ClassBodyFilled _ _ (FunctionsPresent functionDeclarations) _)) =
    functionDeclarations
getFunctionDeclarations _ = []

getActionDeclarations :: ClassDecl -> [ActionDecl]
getActionDeclarations (ClassDeclaration _ _ _ _ (ClassBodyFilled _ _ _ (ActionsPresent actionDeclarations))) =
    actionDeclarations
getActionDeclarations _ = []

valueNamesFromDeclaration :: ClassDecl -> [ObjectIdent]
valueNamesFromDeclaration classDecl = map objectNameFromDeclaration $ getValueDeclarations classDecl

variableNamesFromDeclaration :: ClassDecl -> [ObjectIdent]
variableNamesFromDeclaration classDecl = map objectNameFromDeclaration $ getVariableDeclarations classDecl

attributeNamesFromDeclaration :: ClassDecl -> [ObjectIdent]
attributeNamesFromDeclaration classDecl = valueNamesFromDeclaration classDecl ++ variableNamesFromDeclaration classDecl

classFromObjectType :: ObjectType -> ClassIdent
classFromObjectType (ObjectTypeClass classIdent _) = classIdent

hasMainAction :: ClassDecl -> Bool
hasMainAction = isJust . getMainAction

getMainAction :: ClassDecl -> Maybe ActionDecl
getMainAction (ClassDeclaration _ _ _ _ (ClassBodyFilled _ _ _ (ActionsPresent actions))) =
    List.find isActionMain actions
getMainAction _ = Nothing

singletonInstanceIdentifier :: ClassIdent -> ObjectIdent
singletonInstanceIdentifier (ClassIdentifier (UpperCaseIdent classIdent)) =
    ObjectIdentifier $ LowerCaseIdent $ "__singleton_" ++ classIdent

classIdentifierFromName :: String -> ClassIdent
classIdentifierFromName name = ClassIdentifier (UpperCaseIdent name)

loadClassDeclaration :: ClassDecl -> (ClassIdent, ClassDecl)
loadClassDeclaration declaration = case declaration of
    ClassDeclaration _ _ classIdent _ _ -> (classIdent, declaration)

loadClasses :: [ClassDecl] -> ClassEnv
loadClasses declarations = Map.fromList $ map loadClassDeclaration declarations

findMainClass :: [ClassDecl] -> Maybe ClassDecl
findMainClass = List.find hasMainAction

getConstructorParamList :: ClassDecl -> [ObjectIdent]
getConstructorParamList classDecl = uninitializedValues ++ uninitializedVariables
    where uninitializedValues = map objectNameFromDeclaration $ filter (not . isInitialized) $
            getValueDeclarations classDecl
          uninitializedVariables = map objectNameFromDeclaration $ filter (not . isInitialized) $
            getVariableDeclarations classDecl

getInitializedAttributes :: ClassDecl -> [(ObjectIdent, Expr)]
getInitializedAttributes classDecl = initializedValues ++ initializedVariables
    where initializedValues = map toNameExprPair $ filter isInitialized $ getValueDeclarations classDecl
          initializedVariables = map toNameExprPair $ filter isInitialized $ getVariableDeclarations classDecl

hasAttributeIn :: ObjectType -> MethodIdent -> [ObjectIdent] -> Bool
hasAttributeIn objectType methodIdent attributeNames = attributeIdentifier `elem` attributeNames
    where attributeIdentifier = methodToObjectIdentifier methodIdent
