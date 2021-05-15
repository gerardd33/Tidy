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
type Result = (Object, Env)

type Env = (Object, ClassEnv) -- (localReference, classEnv)
type ClassEnv = Map.Map ClassIdent ClassDecl
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


type TypeEnv = Map.Map String ObjectType
type StaticCheckEnv = (TypeEnv, ObjectType) -- (typeEnv, methodReturnType)
type StaticCheckMonad = ReaderT StaticCheckEnv (ExceptT CompilationError IO)
