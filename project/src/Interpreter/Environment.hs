module Interpreter.Environment where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.Map             as Map
import           Data.Maybe
import           Prelude              hiding (lookup)

import           Parser.Tidy.Abs


type ClassEnv = Map.Map ClassIdent ClassDecl

type StateMonad = ReaderT LocalEnv (StateT RTState (ExceptT RuntimeException IO))

-- TODO These are temporary structures to be replaced
-- with more accurate representations, for now a POC
-- of environments and state handling to be developed iteratively
type Location = Integer
type LocalEnv = Map.Map ValueIdent Location
type RTState = (Map.Map Location Value, Location)

-- TODO other types
data Value = IntValue Integer | BoolValue Boolean | VoidValue
    deriving (Eq, Show)

-- TODO other exceptions
-- TODO rename OtherException to RuntimeException if possible
-- TODO better handling and custom show instance
data RuntimeException =
    DivideByZeroException
    | OtherException
    deriving (Show)

data CompilationError =
    NoMainMethodError
    | OtherError
    deriving (Show)

-- TODO rename later
type Result = (Maybe Value, LocalEnv)


getLocation :: ValueIdent -> StateMonad Location
getLocation identifier = do
    Just location <- asks $ Map.lookup identifier
    return location

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
    returnNothing

addValue :: ValueIdent -> Value -> StateMonad Result
addValue identifier value = do
    localEnv <- ask
    (state, nextLocation) <- get
    put (Map.insert nextLocation value state, nextLocation + 1)
    return (Nothing, Map.insert identifier nextLocation localEnv)

returnNothing :: StateMonad Result
returnNothing = do
    localEnv <- ask
    return (Nothing, localEnv)
