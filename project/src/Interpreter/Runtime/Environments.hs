module Interpreter.Runtime.Environments where

import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.List                             as List
import qualified Data.Map                              as Map
import           Data.Maybe

import           Interpreter.Common.Types
import           Parser.Tidy.Abs

import           Interpreter.Common.Utils.Builtin
import           Interpreter.Common.Utils.Classes
import           Interpreter.Common.Utils.Environments
import           Interpreter.Common.Utils.Methods
import           Interpreter.Common.Utils.Objects
import           Interpreter.Runtime.Types


getClassDeclaration :: ClassIdent -> StateMonad ClassDecl
getClassDeclaration classIdent = do
    (_, classEnv) <- ask
    return $ classEnv Map.! classIdent

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

addLocalObjectAtLocation :: ObjectIdent -> Location -> StateMonad Result
addLocalObjectAtLocation attributeIdent newLocation = do
    (localRef, classEnv) <- ask
    let newValues = Map.insert attributeIdent newLocation (getValues localRef)
    let newLocalRef = newLocalReference $ ObjectEnv newValues (getVariables localRef)
    return (pass, (newLocalRef, classEnv))

addLocalValue :: ObjectIdent -> Object -> StateMonad Result
addLocalValue objectIdent object = do
    location <- allocateObject object
    addLocalObjectAtLocation objectIdent location

addLocalVariable :: ObjectIdent -> Object -> StateMonad Result
addLocalVariable objectIdent object = do
    location <- allocateObject object
    (localRef, classEnv) <- ask
    let newVariables = Map.insert objectIdent location (getVariables localRef)
    let newLocalRef = newLocalReference $ ObjectEnv (getValues localRef) newVariables
    return (pass, (newLocalRef, classEnv))

addLocalValues :: [(ObjectIdent, Object)] -> StateMonad Result
addLocalValues [] = returnPass
addLocalValues (addition:additions) = do
    (_, env) <- uncurry addLocalValue addition
    local (const env) $ addLocalValues additions

getLocalObject :: ObjectIdent -> StateMonad Object
getLocalObject objectIdent = do
    (object, _) <- getLocalObjectWithLocation objectIdent
    return object

getLocalObjectWithLocation :: ObjectIdent -> StateMonad (Object, Location)
getLocalObjectWithLocation objectIdent = do
    (localRef, _) <- ask
    if objectIdent == localReferenceIdentifier then return (localRef, -1)
    else getAttributeWithLocation localRef objectIdent

addArgumentsToEnv :: MethodType -> [Object] -> StateMonad Result
addArgumentsToEnv methodType evaluatedArgs = do
    let methodParamList = getMethodParamNames methodType
    let declarations = zip methodParamList evaluatedArgs
    (_, newEnv) <- addLocalValues declarations
    return (pass, newEnv)

setThisReference :: Object -> StateMonad Result
setThisReference newThisObject = do
     (localRef, _) <- ask
     addLocalValue thisReferenceIdentifier newThisObject

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
    (localRef, _) <- ask
    return $ Map.keys $ getValues localRef

getLocalVariableNames :: StateMonad [ObjectIdent]
getLocalVariableNames = do
    (localRef, _) <- ask
    return $ Map.keys $ getVariables localRef

objectsEqual :: Object -> Object -> StateMonad Bool
objectsEqual object1 object2 = do
    case object1 of BuiltinObject _ -> return $ object1 == object2
                    _               -> complexObjectsEqual object1 object2

complexObjectsEqual :: Object -> Object -> StateMonad Bool
complexObjectsEqual (RegularObject _ env1) (RegularObject _ env2) = do
    attributeValues1 <- mapM retrieveObject $ Map.elems $ Map.union (values env1) (variables env1)
    attributeValues2 <- mapM retrieveObject $ Map.elems $ Map.union (values env2) (variables env2)
    results <- zipWithM objectsEqual attributeValues1 attributeValues2
    return $ and results

objectToString :: Object -> StateMonad String
objectToString (BuiltinObject object) = return $ case object of
    IntObject value    -> show value
    BoolObject value   -> if value == BTrue then "True" else "False"
    CharObject value   -> "'" ++ [value] ++ "'"
    StringObject value -> "\"" ++ value ++ "\""
    VoidObject         -> "Pass"

objectToString (RegularObject objectType objectEnv) = do
    case objectType of
        ObjectTypeClass classIdent _ -> do
            objectEnvString <- objectEnvToString objectEnv
            return $ show objectType ++ " " ++ objectEnvString
-- TODO add method types

objectEnvToString :: ObjectEnv -> StateMonad String
objectEnvToString env = do
    let valueList = Map.toList $ Map.map retrieveObject $ values env
    let variableList = Map.toList $ Map.map retrieveObject $ variables env
    valueStrings <- mapM showAttributeValue valueList
    let valuesString = List.intercalate ", " valueStrings
    variableStrings <- mapM showAttributeValue variableList
    let variablesString = List.intercalate "," variableStrings
    let resultString = "{ " ++ "values: {" ++ valuesString ++ " }, variables: {" ++ variablesString ++ " } }"
    return resultString

showAttributeValue :: (ObjectIdent, StateMonad Object) -> StateMonad String
showAttributeValue (objectIdent, objectCalculation) = do
    object <- objectCalculation
    objectString <- objectToString object
    return $ " " ++ show objectIdent ++ ": " ++ objectString
