module Interpreter.Eval.Environments where

import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.Map                          as Map
import           Data.Maybe

import           Interpreter.Common.Types
import           Parser.Tidy.Abs

import           Interpreter.Common.Helper.Classes
import           Interpreter.Common.Helper.Methods
import           Interpreter.Common.Helper.Objects
import           Interpreter.Eval.Objects
import           Interpreter.Eval.Utils


initialEnvironment :: ClassEnv -> Env
initialEnvironment classEnv = (newLocalObject emptyObjectEnv, emptyThisObject, classEnv)

initialState :: RTState
initialState = (Map.empty, 0)

newLocalObject :: ObjectEnv -> Object
newLocalObject = RegularObject (objectTypeFromClassName "__local")

emptyThisObject :: Object
emptyThisObject = RegularObject (objectTypeFromClassName "__this") emptyObjectEnv

emptyObjectEnv :: ObjectEnv
emptyObjectEnv = ObjectEnv Map.empty Map.empty

allocateObject :: Object -> StateMonad Location
allocateObject object = do
    (state, nextLocation) <- get
    put (Map.insert nextLocation object state, nextLocation + 1)
    return nextLocation

retrieveObject :: Location -> StateMonad Object
retrieveObject location = do
    (state, _) <- get
    return $ fromJust $ Map.lookup location state

setObject :: Location -> Object -> StateMonad Result
setObject location newValue = do
    (state, nextLocation) <- get
    put (Map.insert location newValue state, nextLocation)
    returnPass

addLocalValue :: ObjectIdent -> Object -> StateMonad Result
addLocalValue objectIdent object = do
    location <- allocateObject object
    (localRef, thisRef, classEnv) <- ask
    let newLocalRef = newLocalObject $ ObjectEnv (Map.insert objectIdent location (getValues localRef)) (getVariables localRef)
    return (pass, (newLocalRef, thisRef, classEnv))

addLocalVariable :: ObjectIdent -> Object -> StateMonad Result
addLocalVariable objectIdent object = do
    location <- allocateObject object
    (localRef, thisRef, classEnv) <- ask
    let newLocalRef = newLocalObject $ ObjectEnv (getValues localRef) (Map.insert objectIdent location (getVariables localRef))
    return (pass, (newLocalRef, thisRef, classEnv))

addLocalValues :: [(ObjectIdent, Object)] -> StateMonad Result
addLocalValues [] = returnPass
addLocalValues (addition:additions) = do
    (_, env) <- uncurry addLocalValue addition
    local (const env) $ addLocalValues additions

getLocalAttribute :: ObjectIdent -> StateMonad Object
getLocalAttribute objectIdent = do
    (localRef, _, _) <- ask
    getAttribute localRef objectIdent

addArgumentsToEnv :: MethodType -> [Object] -> StateMonad Result
addArgumentsToEnv methodType evaluatedArgs = do
    let methodParamList = getMethodParamList methodType
    let declarations = zip methodParamList evaluatedArgs
    (_, newEnv) <- addLocalValues declarations
    return (pass, newEnv)

setThisReference :: Object -> StateMonad Result
setThisReference newThisObject = do
    (localRef, _, classEnv) <- ask
    return (pass, (localRef, newThisObject, classEnv))

-- TODO handle builtin objects
getAttribute :: Object -> ObjectIdent -> StateMonad Object
getAttribute (RegularObject _ objectEnv) attributeIdent = do
    if attributeIdent `Map.member` values objectEnv
    then retrieveObject $ values objectEnv Map.! attributeIdent
    else retrieveObject $ variables objectEnv Map.! attributeIdent

setAttribute :: Object -> ObjectIdent -> Object -> StateMonad Result
setAttribute (RegularObject _ objectEnv) attributeIdent newValue = do
    if attributeIdent `Map.member` values objectEnv
    then setObject (values objectEnv Map.! attributeIdent) newValue
    else setObject (variables objectEnv Map.! attributeIdent) newValue

buildSingletonClassInstance :: ClassIdent -> [(ObjectIdent, Object)] -> StateMonad Object
buildSingletonClassInstance classIdent initializedAttributes = do
    let objectType = ObjectTypeClass classIdent GenericParameterAbsent
    objectEnv <- buildObjectEnv objectType [] initializedAttributes
    return $ RegularObject objectType objectEnv

buildObjectEnv :: ObjectType -> [Object] -> [(ObjectIdent, Object)] -> StateMonad ObjectEnv
buildObjectEnv objectType constructorArgs initializedAttributes = do
    attributes <- getAttributeMap objectType constructorArgs initializedAttributes
    objectValueList <- getValueNames objectType
    let (valuesMap, variablesMap) = Map.partitionWithKey (\name _ -> name `elem` objectValueList) attributes
    valuesEnv <- buildAttributeEnv valuesMap
    variablesEnv <- buildAttributeEnv variablesMap
    return $ ObjectEnv valuesEnv variablesEnv

buildAttributeEnv :: Map.Map ObjectIdent Object -> StateMonad AttributeEnv
buildAttributeEnv attributeMap = do
    let (attributeNames, attributeObjects) = unzip $ Map.toList attributeMap
    attributeLocations <- mapM allocateObject attributeObjects
    return $ Map.fromList $ zip attributeNames attributeLocations
