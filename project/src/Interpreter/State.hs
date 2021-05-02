module Interpreter.State where

import           Control.Monad.Except
import           Control.Monad.State
import qualified Data.Map             as Map
import           Data.Maybe
import           Prelude              hiding (lookup)

import           Parser.Tidy.Abs


type ClassEnv = Map.Map ClassIdent ClassDecl

type StateMonad = StateT RTState (ExceptT RuntimeException IO)
type RTState = Map.Map ValueIdent Value

-- TODO other types, especially objects
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


buildInitialState :: RTState
buildInitialState = Map.empty

getValue :: ValueIdent -> StateMonad Value
getValue identifier = gets $ fromJust . Map.lookup identifier

setValue :: ValueIdent -> Value -> StateMonad Value
setValue identifier value = do
    state <- get
    put $ Map.insert identifier value state
    return VoidValue
