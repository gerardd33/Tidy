module Interpreter.Evaluation.Environments where

import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.Map                          as Map
import           Data.Maybe

import           Interpreter.Common.Types
import           Parser.Tidy.Abs

import           Interpreter.Common.Helper.Classes
import           Interpreter.Common.Helper.Methods
import           Interpreter.Common.Helper.Objects
import           Interpreter.Evaluation.Utils


initialEnvironment :: ClassEnv -> Env
initialEnvironment classEnv = (newLocalObject emptyObjectEnv, emptyThisObject, classEnv)

initialState :: RTState
initialState = (Map.empty, 0)

newLocalObject :: ObjectEnv -> Object
newLocalObject = RegularObject localObjectType

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

addLocalAttributeLocation :: ObjectIdent -> Location -> StateMonad Result
addLocalAttributeLocation attributeIdent newLocation = do
    (localRef, thisRef, classEnv) <- ask
    let newValues = Map.insert attributeIdent newLocation (getValues localRef)
    let newLocalRef = newLocalObject $ ObjectEnv newValues (getVariables localRef)
    return (pass, (newLocalRef, thisRef, classEnv))

addLocalValue :: ObjectIdent -> Object -> StateMonad Result
addLocalValue objectIdent object = do
    location <- allocateObject object
    addLocalAttributeLocation objectIdent location

addLocalVariable :: ObjectIdent -> Object -> StateMonad Result
addLocalVariable objectIdent object = do
    location <- allocateObject object
    (localRef, thisRef, classEnv) <- ask
    let newVariables = Map.insert objectIdent location (getVariables localRef)
    let newLocalRef = newLocalObject $ ObjectEnv (getValues localRef) newVariables
    return (pass, (newLocalRef, thisRef, classEnv))

addLocalValues :: [(ObjectIdent, Object)] -> StateMonad Result
addLocalValues [] = returnPass
addLocalValues (addition:additions) = do
    (_, env) <- uncurry addLocalValue addition
    local (const env) $ addLocalValues additions

getLocalAttribute :: ObjectIdent -> StateMonad Object
getLocalAttribute objectIdent = do
    (object, _) <- getLocalAttributeWithLocation objectIdent
    return object

getLocalAttributeWithLocation :: ObjectIdent -> StateMonad (Object, Location)
getLocalAttributeWithLocation objectIdent = do
    (localRef, _, _) <- ask
    if objectIdent == objectIdentifierFromName "local" then return (localRef, -1)
    else getAttributeWithLocation localRef objectIdent

addArgumentsToEnv :: MethodType -> [Object] -> StateMonad Result
addArgumentsToEnv methodType evaluatedArgs = do
    let methodParamList = getMethodParamList methodType
    let declarations = zip methodParamList evaluatedArgs
    (_, newEnv) <- addLocalValues declarations
    return (pass, newEnv)

setThisReference :: Object -> StateMonad Result
setThisReference newThisObject = do
     (localRef, _, _) <- ask
     addLocalValue (objectIdentifierFromName "this") newThisObject

getAttribute :: Object -> ObjectIdent -> StateMonad Object
getAttribute object attributeIdent = do
    (object, _) <- getAttributeWithLocation object attributeIdent
    return object

getAttributeWithLocation :: Object -> ObjectIdent -> StateMonad (Object, Location)
getAttributeWithLocation object attributeIdent = do
    let location = getAttributeLocation object attributeIdent
    object <- retrieveObject location
    return (object, location)

setAttribute :: Object -> ObjectIdent -> Object -> StateMonad Result
setAttribute object attributeIdent newValue = do
    let location = getAttributeLocation object attributeIdent
    setObject location newValue

buildAttributeEnv :: Map.Map ObjectIdent Object -> StateMonad AttributeEnv
buildAttributeEnv attributeMap = do
    let (attributeNames, attributeObjects) = unzip $ Map.toList attributeMap
    attributeLocations <- mapM allocateObject attributeObjects
    return $ Map.fromList $ zip attributeNames attributeLocations

getLocalValueNames :: StateMonad [ObjectIdent]
getLocalValueNames = do
    (localRef, _, _) <- ask
    return $ Map.keys $ getValues localRef

getLocalVariableNames :: StateMonad [ObjectIdent]
getLocalVariableNames = do
    (localRef, _, _) <- ask
    return $ Map.keys $ getVariables localRef
