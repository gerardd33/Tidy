module Interpreter.Common.Utils.Classes where

import qualified Data.List                        as List
import qualified Data.Map                         as Map
import           Data.Maybe

import           Interpreter.Common.Types
import           Parser.Tidy.Abs

import           Interpreter.Common.Utils.Builtin
import           Interpreter.Common.Utils.Methods
import           Interpreter.Common.Utils.Objects
import           Interpreter.Common.Utils.Types


allowedInheritanceConfigurations :: [(ClassTypeModifier, ClassTypeModifier)]
allowedInheritanceConfigurations = [(MSingleton, MSingleton), (MImmutable, MImmutable),
                                    (MMutable, MMutable), (MMutable, MImmutable)]

isInheritanceLegal :: ClassTypeModifier -> ClassTypeModifier -> ClassType -> Bool
isInheritanceLegal classTypeModifier superclassTypeModifier superclassType =
    (classTypeModifier, superclassTypeModifier) `elem` allowedInheritanceConfigurations ||
    (classTypeModifier == MSingleton && superclassType == anyClassType)

getClassIdentifier :: ClassDecl -> ClassIdent
getClassIdentifier (ClassDeclaration _ _ classType _ _) = classIdentifierFromClassType classType

getClassType :: ClassDecl -> ClassType
getClassType (ClassDeclaration _ _ classType _ _) = classType

getSuperclassType :: ClassDecl -> ClassType
getSuperclassType (ClassDeclaration _ _ _ SuperclassAbsent _) = anyClassType
getSuperclassType (ClassDeclaration _ _ _ (SuperclassPresent classType) _) = classType

getClassTypeModifier :: ClassDecl -> ClassTypeModifier
getClassTypeModifier (ClassDeclaration _ typeModifier _ _ _) = typeModifier

getGenericParameterList :: ClassDecl -> [ClassType]
getGenericParameterList = genericParameterListFromClassType . getClassType

genericParameterListFromClassType :: ClassType -> [ClassType]
genericParameterListFromClassType (GeneralClassType _ GenericParameterAbsent) = []
genericParameterListFromClassType (GeneralClassType _ (GenericParameterPresent params)) = params

getValueDeclarations :: ClassDecl -> [ObjectDecl]
getValueDeclarations (ClassDeclaration _ _ _ _ (ClassBodyFilled valuesSection _ _ _)) =
    valueDeclarationsFromValuesSection valuesSection
getValueDeclarations _ = []

valueDeclarationsFromValuesSection :: ValuesSection -> [ObjectDecl]
valueDeclarationsFromValuesSection (ValuesPresent declarations) = declarations
valueDeclarationsFromValuesSection _                            = []

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

attributeNamesFromDeclaration :: ClassDecl -> [ObjectIdent]
attributeNamesFromDeclaration classDecl = valueNamesFromDeclaration classDecl ++ variableNamesFromDeclaration classDecl

methodNamesFromDeclaration :: ClassDecl -> [MethodIdent]
methodNamesFromDeclaration classDecl = functionNamesFromDeclaration classDecl ++ actionNamesFromDeclaration classDecl

valueNamesFromDeclaration :: ClassDecl -> [ObjectIdent]
valueNamesFromDeclaration classDecl = map getObjectIdentifier $ getValueDeclarations classDecl

variableNamesFromDeclaration :: ClassDecl -> [ObjectIdent]
variableNamesFromDeclaration classDecl = map getObjectIdentifier $ getVariableDeclarations classDecl

functionNamesFromDeclaration :: ClassDecl -> [MethodIdent]
functionNamesFromDeclaration classDecl =  map getFunctionIdentifier $ getFunctionDeclarations classDecl

actionNamesFromDeclaration :: ClassDecl -> [MethodIdent]
actionNamesFromDeclaration classDecl = map getActionIdentifier $ getActionDeclarations classDecl

findMainClass :: [ClassDecl] -> Maybe ClassDecl
findMainClass = List.find hasMainAction . List.filter isSingletonClass

hasMainAction :: ClassDecl -> Bool
hasMainAction = isJust . getMainAction

getMainAction :: ClassDecl -> Maybe ActionDecl
getMainAction (ClassDeclaration _ _ _ _ (ClassBodyFilled _ _ _ (ActionsPresent actions))) =
    List.find isActionMain actions
getMainAction _ = Nothing

loadClasses :: [ClassDecl] -> ClassEnv
loadClasses userClasses = Map.fromList $ map loadClassDeclaration $ userClasses ++ builtinClasses

loadClassDeclaration :: ClassDecl -> (ClassIdent, ClassDecl)
loadClassDeclaration declaration = case declaration of
    ClassDeclaration _ _ classType _ _ -> (classIdentifierFromClassType classType, declaration)

classIdentifierFromObjectType :: ObjectType -> ClassIdent
classIdentifierFromObjectType (ObjectTypeClass (GeneralClassType classIdent _)) = classIdent

classIdentifierFromClassType :: ClassType -> ClassIdent
classIdentifierFromClassType (GeneralClassType classIdent _) = classIdent

classNameFromClassType :: ClassType -> String
classNameFromClassType = classNameFromIdentifier . classIdentifierFromClassType

singletonInstanceIdent :: ClassIdent -> ObjectIdent
singletonInstanceIdent classIdent = ObjectIdentifier $ LowerCaseIdent $
    "__singleton_" ++ classNameFromIdentifier classIdent

isSingletonClass :: ClassDecl -> Bool
isSingletonClass = (==MSingleton) . getClassTypeModifier

isBuiltinClass :: ClassType -> Bool
isBuiltinClass classType = classIdent `elem` map classIdentifierFromName builtinClassNames
    where classIdent = classIdentifierFromClassType classType

getConstructorParameterSignatures :: ClassDecl -> [(ObjectIdent, ObjectType)]
getConstructorParameterSignatures classDecl = uninitializedValues ++ uninitializedVariables
    where uninitializedValues = map objectToNameTypePair $ filter (not . isInitialized) $
            getValueDeclarations classDecl
          uninitializedVariables = map objectToNameTypePair $ filter (not . isInitialized) $
            getVariableDeclarations classDecl

getConstructorParameterNames :: ClassDecl -> [ObjectIdent]
getConstructorParameterNames classDecl = map fst $ getConstructorParameterSignatures classDecl

getConstructorParameterTypes :: ClassDecl -> [ObjectType]
getConstructorParameterTypes classDecl = if isBuiltinClass (getClassType classDecl)
    then getConstructorParameterTypesForBuiltinClass $ getClassIdentifier classDecl
    else map snd $ getConstructorParameterSignatures classDecl

getInitializedAttributes :: ClassDecl -> [(ObjectIdent, Expr)]
getInitializedAttributes classDecl = initializedValues ++ initializedVariables
    where initializedValues = map objectToNameExprPair $ filter isInitialized $ getValueDeclarations classDecl
          initializedVariables = map objectToNameExprPair $ filter isInitialized $ getVariableDeclarations classDecl

hasAccessorIn :: MethodIdent -> [ObjectIdent] -> Bool
hasAccessorIn methodIdent attributeNames = attributeIdentifier `elem` attributeNames
    where attributeIdentifier = methodToObjectIdentifier methodIdent

attributeTypeFromClassDeclaration :: ObjectIdent -> ClassDecl -> Maybe ObjectType
attributeTypeFromClassDeclaration attributeIdent classDecl = fmap snd result
    where attributes = map objectToNameTypePair $ getValueDeclarations classDecl ++ getVariableDeclarations classDecl
          result = List.find (\(attributeName, attributeType) -> attributeName == attributeIdent) attributes

functionTypeFromClassDeclaration :: MethodIdent -> ClassDecl -> Maybe MethodType
functionTypeFromClassDeclaration functionIdent classDecl = fmap snd result
    where functions = map functionToNameTypePair $ getFunctionDeclarations classDecl
          result = List.find (\(functionName, functionType) -> functionName == functionIdent) functions

actionTypeFromClassDeclaration :: MethodIdent -> ClassDecl -> Maybe MethodType
actionTypeFromClassDeclaration actionIdent classDecl = fmap snd result
    where actions = map actionToNameTypePair $ getActionDeclarations classDecl
          result = List.find (\(actionName, actionType) -> actionName == actionIdent) actions

emptyClassDeclaration :: ClassType -> ClassDecl
emptyClassDeclaration classType = ClassDeclaration MConcrete MImmutable classType SuperclassAbsent ClassBodyEmpty
