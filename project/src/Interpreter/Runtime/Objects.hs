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
    superclassesInclusive <- getAllSuperclassesInclusive $ classTypeFromObjectType objectType
    let valueNames = concatMap valueNamesFromDeclaration superclassesInclusive
    let (valuesMap, variablesMap) = Map.partitionWithKey (\name _ -> name `elem` valueNames) attributes
    valuesEnv <- buildAttributeEnv valuesMap
    variablesEnv <- buildAttributeEnv variablesMap
    return $ ObjectEnv valuesEnv variablesEnv

buildSingletonClassInstance :: ClassIdent -> [(ObjectIdent, Object)] -> StateMonad Object
buildSingletonClassInstance classIdent initializedAttributes = do
    let objectType = simpleObjectTypeFromClassIdentifier classIdent
    objectEnv <- buildObjectEnv objectType [] initializedAttributes
    return $ RegularObject objectType objectEnv

getAttributeMap :: ObjectType -> [Object] -> [(ObjectIdent, Object)] -> StateMonad (Map.Map ObjectIdent Object)
getAttributeMap objectType constructorArgs initializedAttributes = do
    superclassesInclusive <- getAllSuperclassesInclusive $ classTypeFromObjectType objectType
    let constructorParamList = concatMap getConstructorParameterNames superclassesInclusive
    let attributesFromConstructor = Map.fromList $ zip constructorParamList constructorArgs
    return $ Map.union (Map.fromList initializedAttributes) attributesFromConstructor
