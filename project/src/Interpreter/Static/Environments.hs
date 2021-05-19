module Interpreter.Static.Environments where

import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.Map                         as Map
import           Data.Maybe

import           Interpreter.Common.Types
import           Parser.Tidy.Abs

import           Interpreter.Common.Errors
import           Interpreter.Common.Utils.Builtin
import           Interpreter.Common.Utils.Classes
import           Interpreter.Static.Types


getClassDeclarationStatic :: ClassIdent -> StaticCheckMonad ClassDecl
getClassDeclarationStatic classIdent = do
    (_, classEnv) <- ask
    let lookup = Map.lookup classIdent classEnv
    case lookup of Nothing -> throwError $ ClassNotInScopeError $ showContext classIdent
                   Just classDecl -> return classDecl

setThisReferenceType :: ObjectType -> StaticCheckMonad StaticResult
setThisReferenceType newThisReferenceType = do
    (localEnv, _) <- ask
    addLocalValueType thisReferenceIdentifier newThisReferenceType

checkObjectType :: ObjectType -> StaticCheckMonad ObjectType
checkObjectType objectType = do
    let classIdent = classIdentifierFromObjectType objectType
    classDecl <- getClassDeclarationStatic classIdent
    returnVoid

addLocalValueType :: ObjectIdent -> ObjectType -> StaticCheckMonad StaticResult
addLocalValueType objectIdent objectType = do
    (localEnv, classEnv) <- ask
    let newValues = Map.insert objectIdent objectType (valueTypes localEnv)
    let newLocalEnv = StaticLocalEnv newValues (variableTypes localEnv)
    return (voidType, (newLocalEnv, classEnv))

addLocalVariableType :: ObjectIdent -> ObjectType -> StaticCheckMonad StaticResult
addLocalVariableType objectIdent objectType = do
    (localEnv, classEnv) <- ask
    let newVariables = Map.insert objectIdent objectType (variableTypes localEnv)
    let newLocalEnv = StaticLocalEnv (valueTypes localEnv) newVariables
    return (voidType, (newLocalEnv, classEnv))

tryGetLocalObjectType :: ObjectIdent -> StaticCheckMonad (Maybe ObjectType)
tryGetLocalObjectType objectIdent = do
    (localEnv, _) <- ask
    let localObjectTypes = valueTypes localEnv `Map.union` variableTypes localEnv
    return $ Map.lookup objectIdent localObjectTypes

checkLocalObject :: ObjectIdent -> StaticCheckMonad ObjectType
checkLocalObject objectIdent = do
    lookup <- tryGetLocalObjectType objectIdent
    case lookup of Just objectType -> return objectType
                   Nothing         -> throwError $ ObjectNotInScopeError $ showContext objectIdent

getAttributeTypeStatic :: String -> ObjectType -> ObjectIdent -> StaticCheckMonad ObjectType
getAttributeTypeStatic context objectType attributeIdent = do
    if objectType == localReferenceType then checkLocalObject attributeIdent
    else do let classIdent = classIdentifierFromObjectType objectType
            classDecl <- getClassDeclarationStatic classIdent
            let lookup = attributeTypeFromClassDeclaration classDecl attributeIdent
            case lookup of Nothing -> throwError $ NoSuchAttributeError context (showContext attributeIdent)
                           Just attributeType -> return attributeType

getLocalValueNamesStatic :: StaticCheckMonad [ObjectIdent]
getLocalValueNamesStatic = do
    (localEnv, _) <- ask
    return $ Map.keys $ valueTypes localEnv

getLocalVariableNamesStatic :: StaticCheckMonad [ObjectIdent]
getLocalVariableNamesStatic = do
    (localEnv, _) <- ask
    return $ Map.keys $ variableTypes localEnv

hasGetterStatic :: ObjectType -> MethodIdent -> StaticCheckMonad Bool
hasGetterStatic objectType getterIdent = do
    let classIdent = classIdentifierFromObjectType objectType
    classDecl <- getClassDeclarationStatic classIdent
    localValueNames <- getLocalValueNamesStatic
    localVariableNames <- getLocalVariableNamesStatic
    let localObjects = localValueNames ++ localVariableNames
    let classAttributes = attributeNamesFromDeclaration classDecl
    let attributeNames = if objectType == localReferenceType then localObjects else classAttributes
    return $ hasAccessorIn getterIdent attributeNames

hasSetterStatic :: ObjectType -> MethodIdent -> StaticCheckMonad Bool
hasSetterStatic objectType setterIdent = do
    let classIdent = classIdentifierFromObjectType objectType
    classDecl <- getClassDeclarationStatic classIdent
    localVariables <- getLocalVariableNamesStatic
    let classVariables = variableNamesFromDeclaration classDecl
    let variableNames = if objectType == localReferenceType then localVariables else classVariables
    return $ hasAccessorIn setterIdent variableNames

assertNoPreviousDuplicateDeclaration :: String -> ObjectIdent -> StaticCheckMonad ObjectType
assertNoPreviousDuplicateDeclaration context objectIdent = do
    lookup <- tryGetLocalObjectType objectIdent
    case lookup of Nothing -> returnVoid
                   Just _ -> throwError $ DuplicateDeclarationError (showContext objectIdent) context
