module Interpreter.Static.Environments where

import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.Map                              as Map
import           Data.Maybe

import           Interpreter.Common.Types
import           Parser.Tidy.Abs

import           Interpreter.Common.Errors
import           Interpreter.Common.Utils.Builtin
import           Interpreter.Common.Utils.Classes
import           Interpreter.Common.Utils.Environments
import           Interpreter.Common.Utils.Objects
import           Interpreter.Static.Types


getClassDeclStatic :: ClassIdent -> StaticCheckMonad ClassDecl
getClassDeclStatic classIdent = do
    (_, classEnv) <- ask
    let lookup = Map.lookup classIdent classEnv
    case lookup of Nothing -> throwError $ ClassNotInScopeError $ showContext classIdent
                   Just classDecl -> return classDecl

checkObjectType :: ObjectType -> StaticCheckMonad ObjectType
checkObjectType objectType = do
    let classIdent = classFromObjectType objectType
    classDecl <- getClassDeclStatic classIdent
    returnVoid

addLocalObjectType :: ObjectIdent -> ObjectType -> StaticCheckMonad StaticResult
addLocalObjectType objectIdent objectType = do
    (localEnv, classEnv) <- ask
    let newValues = Map.insert objectIdent objectType (valueTypes localEnv)
    let newLocalEnv = StaticLocalEnv newValues (variableTypes localEnv)
    return (voidType, (newLocalEnv, classEnv))

checkLocalObject :: ObjectIdent -> StaticCheckMonad ObjectType
checkLocalObject objectIdent = do
    (localEnv, _) <- ask
    let localObjectTypes = valueTypes localEnv `Map.union` variableTypes localEnv
    let lookup = Map.lookup objectIdent localObjectTypes
    case lookup of Just objectType -> return objectType
                   Nothing         -> throwError $ ObjectNotInScopeError $ showContext objectIdent

getAttributeTypeStatic :: String -> ObjectType -> ObjectIdent -> StaticCheckMonad ObjectType
getAttributeTypeStatic context objectType attributeIdent = do
    let classIdent = classFromObjectType objectType
    classDecl <- getClassDeclStatic classIdent
    let attributeType = attributeTypeFromClassDeclaration classDecl attributeIdent
    when (isNothing attributeType) $ throwError $ NoSuchAttributeError context (showContext attributeIdent)
    return $ fromJust attributeType

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
    let classIdent = classFromObjectType objectType
    classDecl <- getClassDeclStatic classIdent
    localValueNames <- getLocalValueNamesStatic
    localVariableNames <- getLocalVariableNamesStatic
    let localObjects = localValueNames ++ localVariableNames
    let classAttributes = attributeNamesFromDeclaration classDecl
    let attributeNames = if objectType == localReferenceType then localObjects else classAttributes
    return $ hasAttributeIn getterIdent attributeNames

hasSetterStatic :: ObjectType -> MethodIdent -> StaticCheckMonad Bool
hasSetterStatic objectType setterIdent = do
    let classIdent = classFromObjectType objectType
    classDecl <- getClassDeclStatic classIdent
    localVariables <- getLocalVariableNamesStatic
    let classVariables = variableNamesFromDeclaration classDecl
    let variableNames = if objectType == localReferenceType then localVariables else classVariables
    return $ hasAttributeIn setterIdent variableNames
