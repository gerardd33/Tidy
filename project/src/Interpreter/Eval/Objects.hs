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
    let attributeNames = valueNamesFromDeclaration classDecl ++ variableNamesFromDeclaration classDecl
    hasAttributeIn objectType getterIdent attributeNames

hasSetter :: ObjectType -> MethodIdent -> StateMonad Bool
hasSetter objectType setterIdent = do
    classDecl <- getClassDecl $ classFromObjectType objectType
    hasAttributeIn objectType setterIdent $ variableNamesFromDeclaration classDecl

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
