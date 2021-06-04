module Interpreter.Runtime.Objects where

import           Control.Monad.Reader
import qualified Data.List                        as List
import qualified Data.Map                         as Map
import           Data.Maybe

import           Interpreter.Common.Types
import           Parser.Tidy.Abs

import           Interpreter.Common.Utils.Classes
import           Interpreter.Common.Utils.Methods
import           Interpreter.Common.Utils.Objects
import           Interpreter.Common.Utils.Types
import           Interpreter.Runtime.Classes
import           Interpreter.Runtime.Environments
import           Interpreter.Runtime.Types


buildObjectEnv :: ObjectType -> [Object] -> [(ObjectIdent, Object)] -> StateMonad ObjectEnv
buildObjectEnv objectType constructorArgs initializedAttributes = do
    attributes <- getAttributeMap objectType constructorArgs initializedAttributes
    objectValueList <- getValueNames objectType
    let (valuesMap, variablesMap) = Map.partitionWithKey (\name _ -> name `elem` objectValueList) attributes
    valuesEnv <- buildAttributeEnv valuesMap
    variablesEnv <- buildAttributeEnv variablesMap
    return $ ObjectEnv valuesEnv variablesEnv

buildSingletonClassInstance :: ClassType -> [(ObjectIdent, Object)] -> StateMonad Object
buildSingletonClassInstance classType initializedAttributes = do
    let objectType = ObjectTypeClass classType
    objectEnv <- buildObjectEnv objectType [] initializedAttributes
    return $ RegularObject objectType objectEnv

getAttributeMap :: ObjectType -> [Object] -> [(ObjectIdent, Object)] -> StateMonad (Map.Map ObjectIdent Object)
getAttributeMap objectType constructorArgs initializedAttributes = do
    classDecl <- getClassDeclaration $ classTypeFromObjectType objectType
    let constructorParamList = getConstructorParamNames classDecl
    let attributesFromConstructor = Map.fromList $ zip constructorParamList constructorArgs
    return $ Map.union (Map.fromList initializedAttributes) attributesFromConstructor
