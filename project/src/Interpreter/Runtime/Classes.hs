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
import           Interpreter.Common.Utils.Objects
import           Interpreter.Common.Utils.Types
import           Interpreter.Runtime.Environments


hasGetter :: ObjectType -> MethodIdent -> StateMonad Bool
hasGetter objectType getterIdent = do
    localValueNames <- getLocalValueNames
    localVariableNames <- getLocalVariableNames
    let localObjects = localValueNames ++ localVariableNames
    superclassDeclsInclusive <- getAllSuperclassesInclusive $ classTypeFromObjectType objectType
    let classAttributes = concatMap attributeNamesFromDeclaration superclassDeclsInclusive
    let attributeNames = if objectType == localReferenceType then localObjects else classAttributes
    return $ hasAccessorIn getterIdent attributeNames

hasSetter :: ObjectType -> MethodIdent -> StateMonad Bool
hasSetter objectType setterIdent = do
    classDecl <- getClassDeclaration $ classTypeFromObjectType objectType
    localVariables <- getLocalVariableNames
    superclassDeclsInclusive <- getAllSuperclassesInclusive $ classTypeFromObjectType objectType
    let classVariables = concatMap variableNamesFromDeclaration superclassDeclsInclusive
    let variableNames = if objectType == localReferenceType then localVariables else classVariables
    return $ hasAccessorIn setterIdent variableNames

getMemberFunction :: ObjectType -> MethodIdent -> StateMonad FunctionDecl
getMemberFunction (ObjectTypeClass classType) functionIdent = do
    classDecl <- getClassDeclaration classType
    let functions = getFunctionDeclarations classDecl
    return $ fromJust $ List.find (\f -> getFunctionIdentifier f == functionIdent) functions

getMemberAction :: ObjectType -> MethodIdent -> StateMonad ActionDecl
getMemberAction (ObjectTypeClass classType) actionIdent = do
    classDecl <- getClassDeclaration classType
    let actions = getActionDeclarations classDecl
    return $ fromJust $ List.find (\f -> getActionIdentifier f == actionIdent) actions
