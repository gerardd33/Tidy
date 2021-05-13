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


getValueList :: ObjectType -> StateMonad [ObjectIdent]
getValueList (ObjectTypeClass classIdent _) = do
    classDecl <- getClassDecl classIdent
    return $ getValueNames classDecl

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
    hasAttributeIn objectType getterIdent $ getValueNames classDecl ++ getVariableNames classDecl

hasSetter :: ObjectType -> MethodIdent -> StateMonad Bool
hasSetter objectType setterIdent = do
    classDecl <- getClassDecl $ classFromObjectType objectType
    hasAttributeIn objectType setterIdent $ getVariableNames classDecl

hasAttributeIn :: ObjectType -> MethodIdent -> [ObjectIdent] -> StateMonad Bool
hasAttributeIn objectType methodIdent attributeNames = do
    let attributeIdentifier = methodToObjectIdentifier methodIdent
    return $ attributeIdentifier `elem` attributeNames

buildSingletonClassInstance :: ClassIdent -> [(ObjectIdent, Object)] -> StateMonad Object
buildSingletonClassInstance classIdent initializedAttributes = do
    let objectType = ObjectTypeClass classIdent GenericParameterAbsent
    objectEnv <- buildObjectEnv objectType [] initializedAttributes
    return $ RegularObject objectType objectEnv

buildObjectEnv :: ObjectType -> [Object] -> [(ObjectIdent, Object)] -> StateMonad ObjectEnv
buildObjectEnv objectType constructorArgs initializedAttributes = do
    attributes <- getAttributeMap objectType constructorArgs initializedAttributes
    objectValueList <- getValueList objectType
    let (valuesMap, variablesMap) = Map.partitionWithKey (\name _ -> name `elem` objectValueList) attributes
    valuesEnv <- buildAttributeEnv valuesMap
    variablesEnv <- buildAttributeEnv variablesMap
    return $ ObjectEnv valuesEnv variablesEnv

getAttributeMap :: ObjectType -> [Object] -> [(ObjectIdent, Object)] -> StateMonad (Map.Map ObjectIdent Object)
getAttributeMap objectType constructorArgs initializedAttributes = do
    classDecl <- getClassDecl $ classFromObjectType objectType
    let constructorParamList = getConstructorParamList classDecl
    let attributesFromConstructor = Map.fromList $ zip constructorParamList constructorArgs
    return $ Map.union (Map.fromList initializedAttributes) attributesFromConstructor

buildAttributeEnv :: Map.Map ObjectIdent Object -> StateMonad AttributeEnv
buildAttributeEnv attributeMap = do
    let (attributeNames, attributeObjects) = unzip $ Map.toList attributeMap
    attributeLocations <- mapM allocateObject attributeObjects
    return $ Map.fromList $ zip attributeNames attributeLocations
