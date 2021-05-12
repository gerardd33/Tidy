module Interpreter.Common.Types where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.Map                  as Map

import           Parser.Tidy.Abs

import           Interpreter.Common.Debug
import           Interpreter.Common.Errors


type StateMonad = ReaderT Env (StateT RTState (ExceptT RuntimeException IO))
type RTState = (Map.Map Location Object, Location)

type Env = (LocalEnv, ClassEnv)
type LocalEnv = Map.Map ObjectIdent Location
type ClassEnv = Map.Map ClassIdent ClassDecl

type Result = (Object, Env)
type Location = Integer

data Object = RegularObject ObjectType ObjectEnv | BuiltinObjectObject BuiltinObject
    deriving (Eq, Show)

data ObjectEnv = ObjectEnv { values :: ValueEnv, variables :: ValueEnv }
    deriving (Eq, Show)

type ValueEnv = Map.Map ObjectIdent Object

data BuiltinObject
    = IntObject Integer
    | BoolObject Boolean
    | CharObject Char
    | StringObject [Char]
    | VoidObject
    deriving (Eq, Show)
