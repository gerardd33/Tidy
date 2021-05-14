module Interpreter.Runtime.Methods where

import           Control.Monad.Reader
import qualified Data.Map                          as Map

import           Interpreter.Common.Types
import           Parser.Tidy.Abs

import           Interpreter.Common.Utils.Objects
import           Interpreter.Runtime.Environments


evaluateGetter :: Object -> MethodIdent -> StateMonad Object
evaluateGetter object functionIdent = do
    let attributeIdent = methodToObjectIdentifier functionIdent
    getAttribute object attributeIdent

evaluateSetter :: Object -> MethodIdent -> Object -> StateMonad Result
evaluateSetter object actionIdent newValue = do
    let attributeIdent = methodToObjectIdentifier actionIdent
    setAttribute object attributeIdent newValue
