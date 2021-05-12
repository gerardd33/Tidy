module Interpreter.Eval.LocalEnvironment where

import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.Map                        as Map
import           Data.Maybe

import           Interpreter.Common.Helper.Types
import           Interpreter.Common.Types
import           Interpreter.Eval.Utils
import           Parser.Tidy.Abs


getLocation :: ObjectIdent -> StateMonad Location
getLocation objectIdent = do
    (localEnv, _) <- ask
    return $ fromJust $ Map.lookup objectIdent localEnv

getLocalValue :: ObjectIdent -> StateMonad Object
getLocalValue objectIdent = do
    location <- getLocation objectIdent
    (state, _) <- get
    return $ fromJust $ Map.lookup location state

addLocalValue :: ObjectIdent -> Object -> StateMonad Result
addLocalValue objectIdent object = do
    (localEnv, classEnv) <- ask
    (state, nextLocation) <- get
    put (Map.insert nextLocation object state, nextLocation + 1)
    return (pass, (Map.insert objectIdent nextLocation localEnv, classEnv))






buildInitialEnvironment :: ClassEnv -> Env
buildInitialEnvironment classEnv = (Map.empty, classEnv)

buildInitialState :: RTState
buildInitialState = (Map.empty, 0)

setObject :: ObjectIdent -> Object -> StateMonad Result
setObject identifier value = do
    location <- getLocation identifier
    (state, nextLocation) <- get
    put (Map.insert location value state, nextLocation)
    returnPass

executeObjectAdditions :: [(ObjectIdent, Object)] -> StateMonad Result
executeObjectAdditions [] = returnPass
executeObjectAdditions (addition:additions) = do
    (_, env) <- uncurry addLocalValue addition
    local (const env) $ executeObjectAdditions additions
