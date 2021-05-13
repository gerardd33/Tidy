module Interpreter.Eval.Environments where

import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.Map                          as Map
import           Data.Maybe

import           Interpreter.Common.Types
import           Parser.Tidy.Abs

import           Interpreter.Common.Helper.Objects
import           Interpreter.Eval.Utils


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
    (localEnv, classEnv) <- ask
    return (pass, (Map.insert objectIdent location localEnv, classEnv))

addLocalValues :: [(ObjectIdent, Object)] -> StateMonad Result
addLocalValues [] = returnPass
addLocalValues (addition:additions) = do
    (_, env) <- uncurry addLocalValue addition
    local (const env) $ addLocalValues additions

getLocalValueLocation :: ObjectIdent -> StateMonad Location
getLocalValueLocation objectIdent = do
    (localEnv, _) <- ask
    return $ fromJust $ Map.lookup objectIdent localEnv

getLocalValue :: ObjectIdent -> StateMonad Object
getLocalValue objectIdent = do
    location <- getLocalValueLocation objectIdent
    (state, _) <- get
    return $ fromJust $ Map.lookup location state
