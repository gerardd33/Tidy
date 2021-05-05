module Interpreter.Common.Types where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.Map                  as Map

import           Interpreter.Common.Errors
import           Interpreter.Common.Utils
import           Parser.Tidy.Abs


type StateMonad = ReaderT Env (StateT RTState (ExceptT RuntimeException IO))
type RTState = (Map.Map Location Value, Location)

type Env = (LocalEnv, ClassEnv)
type LocalEnv = Map.Map ValueIdent Location
type ClassEnv = Map.Map ClassIdent ClassDecl

type Result = (Value, Env)
type Location = Integer

-- TODO better printing (custom show instance)
data Value = RegularObject ValueType ObjectEnv | SingleValueObject SingleValue
    deriving (Eq, Show)

data ObjectEnv = ObjectEnv { values :: ValueEnv, variables :: ValueEnv }
    deriving (Eq, Show)

type ValueEnv = Map.Map ValueIdent Value

data SingleValue = IntValue Integer
    | BoolValue Boolean
    | CharValue Char
    | StringValue [Char]
    | VoidValue
    deriving (Eq, Show)
