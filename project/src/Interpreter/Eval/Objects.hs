module Interpreter.Eval.Objects where

import           Control.Monad.Reader
import qualified Data.List                         as List
import qualified Data.Map                          as Map
import           Data.Maybe

import           Interpreter.Common.Types
import           Parser.Tidy.Abs

import           Interpreter.Common.Helper.Classes
import           Interpreter.Common.Helper.Methods
import           Interpreter.Common.Helper.Objects
import           Interpreter.Common.Helper.Types
import           Interpreter.Eval.Environments
import           Interpreter.Eval.Utils


getValueNames :: ObjectType -> StateMonad [ObjectIdent]
getValueNames (ObjectTypeClass classIdent _) = do
    classDecl <- getClassDecl classIdent
    return $ valueNamesFromDeclaration classDecl

getMemberFunction :: ObjectType -> MethodIdent -> StateMonad FunctionDecl
getMemberFunction (ObjectTypeClass classIdent _) functionIdent = do
    classDecl <- getClassDecl classIdent
    let functions = getFunctionDeclarations classDecl
    return $ fromJust $ List.find (\f -> getFunctionIdentifier f == functionIdent) functions

getMemberAction :: ObjectType -> MethodIdent -> StateMonad ActionDecl
getMemberAction (ObjectTypeClass classIdent _) actionIdent = do
    classDecl <- getClassDecl classIdent
    let actions = getActionDeclarations classDecl
    return $ fromJust $ List.find (\f -> getActionIdentifier f == actionIdent) actions

hasGetter :: ObjectType -> MethodIdent -> StateMonad Bool
hasGetter objectType getterIdent = do
    classDecl <- getClassDecl $ classFromObjectType objectType
    localValueNames <- getLocalValueNames
    localVariableNames <- getLocalVariableNames
    let localAttributes = localValueNames ++ localVariableNames
    let classAttributes = valueNamesFromDeclaration classDecl ++ variableNamesFromDeclaration classDecl
    let attributeNames = if objectType == localObjectType then localAttributes else classAttributes
    hasAttributeIn objectType getterIdent attributeNames

hasSetter :: ObjectType -> MethodIdent -> StateMonad Bool
hasSetter objectType setterIdent = do
    classDecl <- getClassDecl $ classFromObjectType objectType
    localVariables <- getLocalVariableNames
    let classVariables = variableNamesFromDeclaration classDecl
    let variableNames = if objectType == localObjectType then localVariables else classVariables
    hasAttributeIn objectType setterIdent variableNames

hasAttributeIn :: ObjectType -> MethodIdent -> [ObjectIdent] -> StateMonad Bool
hasAttributeIn objectType methodIdent attributeNames = do
    let attributeIdentifier = methodToObjectIdentifier methodIdent
    return $ attributeIdentifier `elem` attributeNames

getAttributeMap :: ObjectType -> [Object] -> [(ObjectIdent, Object)] -> StateMonad (Map.Map ObjectIdent Object)
getAttributeMap objectType constructorArgs initializedAttributes = do
    classDecl <- getClassDecl $ classFromObjectType objectType
    let constructorParamList = getConstructorParamList classDecl
    let attributesFromConstructor = Map.fromList $ zip constructorParamList constructorArgs
    return $ Map.union (Map.fromList initializedAttributes) attributesFromConstructor

buildSingletonClassInstance :: ClassIdent -> [(ObjectIdent, Object)] -> StateMonad Object
buildSingletonClassInstance classIdent initializedAttributes = do
    let objectType = ObjectTypeClass classIdent GenericParameterAbsent
    objectEnv <- buildObjectEnv objectType [] initializedAttributes
    return $ RegularObject objectType objectEnv

buildObjectEnv :: ObjectType -> [Object] -> [(ObjectIdent, Object)] -> StateMonad ObjectEnv
buildObjectEnv objectType constructorArgs initializedAttributes = do
    attributes <- getAttributeMap objectType constructorArgs initializedAttributes
    objectValueList <- getValueNames objectType
    let (valuesMap, variablesMap) = Map.partitionWithKey (\name _ -> name `elem` objectValueList) attributes
    valuesEnv <- buildAttributeEnv valuesMap
    variablesEnv <- buildAttributeEnv variablesMap
    return $ ObjectEnv valuesEnv variablesEnv
