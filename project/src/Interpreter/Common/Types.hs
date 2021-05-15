module Interpreter.Common.Types where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.Map                  as Map

import           Parser.Tidy.Abs

import           Interpreter.Common.Debug
import           Interpreter.Common.Errors


type StateMonad = ReaderT Env (StateT RTState (ExceptT RuntimeException IO))
type Env = (Object, ClassEnv) -- (localReference, classEnv)
type ClassEnv = Map.Map ClassIdent ClassDecl
type RTState = (Map.Map Location Object, Location)

type Result = (Object, Env)
type Location = Integer

data Object = RegularObject ObjectType ObjectEnv | BuiltinObject BuiltinObject
    deriving (Eq, Show)

data ObjectEnv = ObjectEnv { values :: AttributeEnv, variables :: AttributeEnv }
    deriving (Eq, Show)

type AttributeEnv = Map.Map ObjectIdent Location

data BuiltinObject
    = IntObject Integer
    | BoolObject Boolean
    | CharObject Char
    | StringObject String
    | VoidObject
    deriving (Eq, Show)


type StaticCheckMonad = ReaderT StaticEnv (ExceptT CompilationError IO)
type StaticEnv = (StaticLocalEnv, ClassEnv)
type StaticResult = (ObjectType, StaticEnv)

data StaticLocalEnv = StaticLocalEnv { valuesStatic :: StaticAttributeEnv, variablesStatic :: StaticAttributeEnv }
    deriving (Eq, Show)

type StaticAttributeEnv = Map.Map ObjectIdent ObjectType
