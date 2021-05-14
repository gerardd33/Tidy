module Interpreter.Runtime.Classes where

import           Control.Monad.Reader
import qualified Data.List                           as List
import qualified Data.Map                            as Map
import           Data.Maybe

import           Interpreter.Common.Types
import           Parser.Tidy.Abs

import           Interpreter.Common.Utils.Classes
import           Interpreter.Common.Utils.Methods
import           Interpreter.Common.Utils.Objects
import           Interpreter.Runtime.Environments


getClassDecl :: ClassIdent -> StateMonad ClassDecl
getClassDecl classIdent = do
    (_, classEnv) <- ask
    return $ classEnv Map.! classIdent

hasGetter :: ObjectType -> MethodIdent -> StateMonad Bool
hasGetter objectType getterIdent = do
    classDecl <- getClassDecl $ classFromObjectType objectType
    localValueNames <- getLocalValueNames
    localVariableNames <- getLocalVariableNames
    let localObjects = localValueNames ++ localVariableNames
    let classAttributes = valueNamesFromDeclaration classDecl ++ variableNamesFromDeclaration classDecl
    let attributeNames = if objectType == localReferenceType then localObjects else classAttributes
    hasAttributeIn objectType getterIdent attributeNames

hasSetter :: ObjectType -> MethodIdent -> StateMonad Bool
hasSetter objectType setterIdent = do
    classDecl <- getClassDecl $ classFromObjectType objectType
    localVariables <- getLocalVariableNames
    let classVariables = variableNamesFromDeclaration classDecl
    let variableNames = if objectType == localReferenceType then localVariables else classVariables
    hasAttributeIn objectType setterIdent variableNames

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

hasAttributeIn :: ObjectType -> MethodIdent -> [ObjectIdent] -> StateMonad Bool
hasAttributeIn objectType methodIdent attributeNames = do
    let attributeIdentifier = methodToObjectIdentifier methodIdent
    return $ attributeIdentifier `elem` attributeNames
