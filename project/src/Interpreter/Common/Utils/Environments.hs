module Interpreter.Common.Utils.Environments where

import qualified Data.Map                         as Map

import           Interpreter.Common.Types
import           Parser.Tidy.Abs

import           Interpreter.Common.Utils.Objects


initialEnvironment :: ClassEnv -> Env
initialEnvironment classEnv = (newLocalReference emptyObjectEnv, classEnv)

initialStaticEnvironment :: ClassEnv -> StaticEnv
initialStaticEnvironment classEnv = (emptyStaticLocalEnv, classEnv)

emptyStaticLocalEnv :: StaticLocalEnv
emptyStaticLocalEnv = StaticLocalEnv Map.empty Map.empty

initialState :: RTState
initialState = (Map.empty, 0)

newLocalReference :: ObjectEnv -> Object
newLocalReference = RegularObject localReferenceType

emptyObjectEnv :: ObjectEnv
emptyObjectEnv = ObjectEnv Map.empty Map.empty
