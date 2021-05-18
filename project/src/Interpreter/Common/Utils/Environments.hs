module Interpreter.Common.Utils.Environments where

import qualified Data.Map                         as Map

import           Interpreter.Common.Types
import           Parser.Tidy.Abs

import           Interpreter.Common.Utils.Builtin
import           Interpreter.Common.Utils.Types


initialEnvironment :: ClassEnv -> Env
initialEnvironment classEnv = (newLocalReference emptyObjectEnv, classEnv)

initialStaticEnvironment :: ClassEnv -> StaticEnv
initialStaticEnvironment classEnv = (initialStaticLocalEnv, classEnv)

initialStaticLocalEnv :: StaticLocalEnv
initialStaticLocalEnv = StaticLocalEnv Map.empty
    (Map.fromList [(localReferenceIdentifier, localReferenceType)])

initialState :: RTState
initialState = (Map.empty, 0)

newLocalReference :: ObjectEnv -> Object
newLocalReference = RegularObject localReferenceType

emptyObjectEnv :: ObjectEnv
emptyObjectEnv = ObjectEnv Map.empty Map.empty
