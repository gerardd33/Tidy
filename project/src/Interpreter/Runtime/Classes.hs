module Interpreter.Runtime.Classes where

import           Control.Monad.Reader
import qualified Data.List                        as List
import qualified Data.Map                         as Map
import           Data.Maybe

import           Interpreter.Common.Types
import           Parser.Tidy.Abs

import           Interpreter.Common.Utils.Builtin
import           Interpreter.Common.Utils.Classes
import           Interpreter.Common.Utils.Methods
import           Interpreter.Runtime.Environments


hasGetter :: ObjectType -> MethodIdent -> StateMonad Bool
hasGetter objectType getterIdent = do
    classDecl <- getClassDeclaration $ classIdentifierFromObjectType objectType
    localValueNames <- getLocalValueNames
    localVariableNames <- getLocalVariableNames
    let localObjects = localValueNames ++ localVariableNames
    let classAttributes = attributeNamesFromDeclaration classDecl
    let attributeNames = if objectType == localReferenceType then localObjects else classAttributes
    return $ hasAccessorIn getterIdent attributeNames

hasSetter :: ObjectType -> MethodIdent -> StateMonad Bool
hasSetter objectType setterIdent = do
    classDecl <- getClassDeclaration $ classIdentifierFromObjectType objectType
    localVariables <- getLocalVariableNames
    let classVariables = variableNamesFromDeclaration classDecl
    let variableNames = if objectType == localReferenceType then localVariables else classVariables
    return $ hasAccessorIn setterIdent variableNames

getValueNames :: ObjectType -> StateMonad [ObjectIdent]
getValueNames (ObjectTypeClass classIdent _) = do
    classDecl <- getClassDeclaration classIdent
    return $ valueNamesFromDeclaration classDecl

getMemberFunction :: ObjectType -> MethodIdent -> StateMonad FunctionDecl
getMemberFunction (ObjectTypeClass classIdent _) functionIdent = do
    classDecl <- getClassDeclaration classIdent
    let functions = getFunctionDeclarations classDecl
    return $ fromJust $ List.find (\f -> getFunctionIdentifier f == functionIdent) functions

getMemberAction :: ObjectType -> MethodIdent -> StateMonad ActionDecl
getMemberAction (ObjectTypeClass classIdent _) actionIdent = do
    classDecl <- getClassDeclaration classIdent
    let actions = getActionDeclarations classDecl
    return $ fromJust $ List.find (\f -> getActionIdentifier f == actionIdent) actions
