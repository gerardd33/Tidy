module Interpreter.Eval.LocalEnvironment where

import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.Map                          as Map
import           Data.Maybe

import           Interpreter.Common.Types
import           Parser.Tidy.Abs

import           Interpreter.Common.Helper.Objects
import           Interpreter.Eval.Utils


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

addLocalValues :: [(ObjectIdent, Object)] -> StateMonad Result
addLocalValues [] = returnPass
addLocalValues (addition:additions) = do
    (_, env) <- uncurry addLocalValue addition
    local (const env) $ addLocalValues additions
