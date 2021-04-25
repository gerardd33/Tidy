module Interpreter.Environment where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.Map             as Map
import           Data.Maybe
import           Parser.Tidy.Abs
import           Prelude              hiding (lookup)


type ClassEnv = Map.Map String ClassDecl

type StateMonad = ReaderT Env (StateT Store (ExceptT RuntimeException IO))

-- TODO These are temporary structures to be replaced
-- with more accurate representations, for now a POC
-- of environments and state handling to be developed iteratively
type Identifier = String
type Location = Integer
type Env = Map.Map Identifier Location
type Store = (Map.Map Location StoredValue, Location)

-- TODO other types
data StoredValue = IntValue Integer | BoolValue Bool
    deriving (Eq, Show)

-- TODO other exceptions
-- TODO rename OtherException to RuntimeException if possible
data RuntimeException =
    DivideByZeroException
    | OtherException
    deriving (Show)

-- TODO remove/simplify?
type Result = (Maybe StoredValue, Env)


getLocation :: Identifier -> StateMonad Location
getLocation identifier = do
    Just location <- asks $ Map.lookup identifier
    return location

getValue :: Identifier -> StateMonad StoredValue
getValue identifier = do
    location <- getLocation identifier
    (state, _) <- get
    return $ fromJust $ Map.lookup location state

setValue :: Identifier -> StoredValue -> StateMonad Result
setValue identifier value = do
    location <- getLocation identifier
    (state, nextLocation) <- get
    put (Map.insert location value state, nextLocation)
    returnNow

addValue :: Identifier -> StoredValue -> StateMonad Result
addValue identifier value = do
    env <- ask
    (state, nextLocation) <- get
    put (Map.insert nextLocation value state, nextLocation + 1)
    return (Nothing, Map.insert identifier nextLocation env)

returnNow :: StateMonad Result
returnNow = do
    env <- ask
    return (Nothing, env)
