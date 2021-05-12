module Interpreter.Eval.LocalEnvironment where

import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.Map                        as Map
import           Data.Maybe

import           Interpreter.Common.Helper.Types
import           Interpreter.Common.Types
import           Parser.Tidy.Abs


getLocation :: ObjectIdent -> StateMonad Location
getLocation objectName = do
    (localEnv, _) <- ask
    return $ fromJust $ Map.lookup objectName localEnv

getLocalValue :: ObjectIdent -> StateMonad Object
getLocalValue objectName = do
    location <- getLocation objectName
    (state, _) <- get
    return $ fromJust $ Map.lookup location state

addLocalValue :: ObjectIdent -> Object -> StateMonad Result
addLocalValue objectName object = do
    (localEnv, classEnv) <- ask
    (state, nextLocation) <- get
    put (Map.insert nextLocation object state, nextLocation + 1)
    return (pass, (Map.insert objectName nextLocation localEnv, classEnv))
