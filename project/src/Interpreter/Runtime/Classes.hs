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
    superclassesInclusive <- getAllSuperclassesInclusive $ classTypeFromObjectType objectType
    let classAttributes = concatMap attributeNamesFromDeclaration superclassesInclusive
    let attributeNames = if objectType == localReferenceType then localObjects else classAttributes
    return $ hasAccessorIn getterIdent attributeNames

hasSetter :: ObjectType -> MethodIdent -> StateMonad Bool
hasSetter objectType setterIdent = do
    classDecl <- getClassDeclaration $ classTypeFromObjectType objectType
    localVariables <- getLocalVariableNames
    superclassesInclusive <- getAllSuperclassesInclusive $ classTypeFromObjectType objectType
    let classVariables = concatMap variableNamesFromDeclaration superclassesInclusive
    let variableNames = if objectType == localReferenceType then localVariables else classVariables
    return $ hasAccessorIn setterIdent variableNames

getMemberFunction :: ObjectType -> MethodIdent -> StateMonad FunctionDecl
getMemberFunction (ObjectTypeClass classType) functionIdent = do
    superclassesInclusive <- getAllSuperclassesInclusive classType
    let functions = concatMap getFunctionDeclarations superclassesInclusive
    return $ fromJust $ List.find (\f -> getFunctionIdentifier f == functionIdent) functions

getMemberAction :: ObjectType -> MethodIdent -> StateMonad ActionDecl
getMemberAction (ObjectTypeClass classType) actionIdent = do
    superclassesInclusive <- getAllSuperclassesInclusive classType
    let actions = concatMap getActionDeclarations superclassesInclusive
    return $ fromJust $ List.find (\f -> getActionIdentifier f == actionIdent) actions
