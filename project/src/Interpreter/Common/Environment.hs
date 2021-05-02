module Interpreter.Common.Environment where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.Map             as Map
import           Data.Maybe
import           Prelude              hiding (lookup)

import           Parser.Tidy.Abs


type StateMonad = ReaderT Env (StateT RTState (ExceptT RuntimeException IO))
type RTState = (Map.Map Location Value, Location)

type Env = (LocalEnv, ClassEnv)
type LocalEnv = Map.Map ValueIdent Location
type ClassEnv = Map.Map ClassIdent ClassDecl

type Result = (Value, Env)
type Location = Integer

-- TODO other types, especially proper objects
data Value = IntValue Integer
    | BoolValue Boolean
    | CharValue Char
    | StringValue String
    | VoidValue
    deriving (Eq, Show)

-- TODO other exceptions
-- TODO rename OtherException to RuntimeException if possible
-- TODO better handling and custom show instance
data RuntimeException = DivideByZeroException
    | OtherException
    deriving (Show)

data CompilationError = NoMainMethodError
    | OtherError
    deriving (Show)


buildInitialEnv :: ClassEnv -> Env
buildInitialEnv classEnv = (Map.empty, classEnv)

buildInitialState :: RTState
buildInitialState = (Map.empty, 0)

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
    returnVoid

addValue :: ValueIdent -> Value -> StateMonad Result
addValue identifier value = do
    (localEnv, classEnv) <- ask
    (state, nextLocation) <- get
    put (Map.insert nextLocation value state, nextLocation + 1)
    return (VoidValue, (Map.insert identifier nextLocation localEnv, classEnv))

returnVoid :: StateMonad Result
returnVoid = do
    env <- ask
    return (VoidValue, env)

returnPure :: StateMonad Value -> StateMonad Result
returnPure function = do
    env <- ask
    value <- function
    return (value, env)
