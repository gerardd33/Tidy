module Interpreter.Eval.Environment where

import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.Map                 as Map
import           Data.Maybe

import           Interpreter.Common.Types
import           Interpreter.Eval.Objects
import           Parser.Tidy.Abs


buildInitialEnv :: ClassEnv -> Env
buildInitialEnv classEnv = (Map.empty, classEnv)

buildInitialState :: RTState
buildInitialState = (Map.empty, 0)

emptyValueEnv :: ValueEnv
emptyValueEnv = Map.empty

getLocation :: ValueIdent -> StateMonad Location
getLocation identifier = do
    (localEnv, _) <- ask
    return $ fromJust $ Map.lookup identifier localEnv

getValue :: ValueIdent -> StateMonad Value
getValue identifier = do
    location <- getLocation identifier
    (state, _) <- get
    return $ fromJust $ Map.lookup location state

setValue :: ValueIdent -> Value -> StateMonad Result
setValue identifier value = do
    location <- getLocation identifier
    (state, nextLocation) <- get
    put (Map.insert location value state, nextLocation)
    returnPass

addValue :: ValueIdent -> Value -> StateMonad Result
addValue identifier value = do
    (localEnv, classEnv) <- ask
    (state, nextLocation) <- get
    put (Map.insert nextLocation value state, nextLocation + 1)
    return (pass, (Map.insert identifier nextLocation localEnv, classEnv))

returnPass :: StateMonad Result
returnPass = do
    env <- ask
    return (pass, env)

returnPure :: StateMonad Value -> StateMonad Result
returnPure function = do
    env <- ask
    value <- function
    return (value, env)
