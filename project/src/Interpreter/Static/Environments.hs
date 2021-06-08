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
import           Interpreter.Common.Utils.Objects
import           Interpreter.Common.Utils.Types
import           Interpreter.Static.Types


getClassDeclarationStatic :: ClassType -> StaticCheckMonad ClassDecl
getClassDeclarationStatic classType = do
    (_, classEnv) <- ask
    let classIdent = classIdentifierFromClassType classType
    let lookup = Map.lookup classIdent classEnv
    case lookup of Nothing -> throwError $ ClassNotInScopeError $ show classIdent
                   Just classDecl -> return classDecl

getAllSuperclassesInclusiveStatic :: ClassType -> StaticCheckMonad [ClassDecl]
getAllSuperclassesInclusiveStatic = collectAllSuperclassesInclusiveStatic []

collectAllSuperclassesInclusiveStatic :: [ClassDecl] -> ClassType -> StaticCheckMonad [ClassDecl]
collectAllSuperclassesInclusiveStatic collected classType = do
    classDecl <- getClassDeclarationStatic classType
    if classType == anyClassType then return (classDecl:collected)
    else do let superclassType = getSuperclassType classDecl
            collectAllSuperclassesInclusiveStatic (classDecl:collected) superclassType

setThisReferenceType :: ObjectType -> StaticCheckMonad StaticResult
setThisReferenceType newThisReferenceType = do
    (localEnv, _) <- ask
    addLocalValueType thisReferenceIdentifier newThisReferenceType

checkObjectType :: ObjectType -> StaticCheckMonad ObjectType
checkObjectType objectType = do
    let classType = classTypeFromObjectType objectType
    classDecl <- getClassDeclarationStatic classType
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

registerEmptyClassesInEnv :: [ClassType] -> StaticCheckMonad StaticResult
registerEmptyClassesInEnv [] = liftPureStatic returnVoid
registerEmptyClassesInEnv (addition:additions) = do
    (_, env) <- registerEmptyClassInEnv addition
    local (const env) $ registerEmptyClassesInEnv additions

registerEmptyClassInEnv :: ClassType -> StaticCheckMonad StaticResult
registerEmptyClassInEnv classType = do
    (localEnv, classEnv) <- ask
    let newClassEnv = Map.insert (classIdentifierFromClassType classType) (emptyClassDeclaration classType) classEnv
    return (voidType, (localEnv, newClassEnv))

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
    else do let classType = classTypeFromObjectType objectType
            superclassesInclusive <- getAllSuperclassesInclusiveStatic $ classTypeFromObjectType objectType
            let found = filter isJust $ map (attributeTypeFromClassDeclaration attributeIdent) superclassesInclusive
            case found of [] -> throwError $ NoSuchAttributeError context (showContext attributeIdent)
                          (Just attributeType):_ -> return attributeType

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
    superclassesInclusive <- getAllSuperclassesInclusiveStatic $ classTypeFromObjectType objectType
    let classAttributes = concatMap attributeNamesFromDeclaration superclassesInclusive
    localValueNames <- getLocalValueNamesStatic
    localVariableNames <- getLocalVariableNamesStatic
    let localObjects = localValueNames ++ localVariableNames
    let attributeNames = if objectType == localReferenceType then localObjects else classAttributes
    return $ hasAccessorIn getterIdent attributeNames

hasSetterStatic :: ObjectType -> MethodIdent -> StaticCheckMonad Bool
hasSetterStatic objectType setterIdent = do
    superclassesInclusive <- getAllSuperclassesInclusiveStatic $ classTypeFromObjectType objectType
    let classVariables = concatMap variableNamesFromDeclaration superclassesInclusive
    localVariables <- getLocalVariableNamesStatic
    let variableNames = if objectType == localReferenceType then localVariables else classVariables
    return $ hasAccessorIn setterIdent variableNames

assertNoPreviousDuplicateDeclaration :: String -> ObjectIdent -> StaticCheckMonad ObjectType
assertNoPreviousDuplicateDeclaration context objectIdent = do
    lookup <- tryGetLocalObjectType objectIdent
    case lookup of Nothing -> returnVoid
                   Just _ -> throwError $ DuplicateDeclarationError (showContext objectIdent) context

checkAttributeRepetitions :: String -> ClassType -> StaticCheckMonad ObjectType
checkAttributeRepetitions context classType = do
    superclassesInclusive <- getAllSuperclassesInclusiveStatic classType
    let attributeNames = concatMap attributeNamesFromDeclaration superclassesInclusive
    assertNoDeclarationRepetitions context attributeNames
