module Interpreter.Eval.Objects where

import           Control.Monad.Reader
import qualified Data.List                         as List
import qualified Data.Map                          as Map
import           Data.Maybe

import           Interpreter.Common.Types
import           Parser.Tidy.Abs

import           Interpreter.Common.Helper.Objects
import           Interpreter.Common.Helper.Classes
import           Interpreter.Common.Helper.Methods
import           Interpreter.Common.Helper.Types
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

hasGetter :: ObjectType -> MethodIdent -> StateMonad Bool
hasGetter objectType functionIdent = do
    classDecl <- getClassDecl $ classFromObjectType objectType
    let attributeIdentifier = methodToObjectIdentifier functionIdent
    let attributes = getValueNames classDecl ++ getVariableNames classDecl
    return $ attributeIdentifier `elem` attributes

buildSingletonClassInstance :: ClassIdent -> [(ObjectIdent, Object)] -> StateMonad Object
buildSingletonClassInstance classIdent initializedAttributes = do
    let objectType = ObjectTypeClass classIdent GenericParameterAbsent
    objectEnv <- buildObjectEnv objectType [] initializedAttributes
    return $ RegularObject objectType objectEnv

buildObjectEnv :: ObjectType -> [Object] -> [(ObjectIdent, Object)] -> StateMonad ObjectEnv
buildObjectEnv objectType args initializedAttributes = do
    classDecl <- getClassDecl $ classFromObjectType objectType
    let constructorParamList = getConstructorParamList classDecl
    let attributesFromConstructor = Map.fromList $ zip constructorParamList args
    let attributes = Map.union (Map.fromList initializedAttributes) attributesFromConstructor
    objectValueList <- getValueList objectType
    let (values, variables) = Map.partitionWithKey (\name _ -> name `elem` objectValueList) attributes
    return $ ObjectEnv values variables
