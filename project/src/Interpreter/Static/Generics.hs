module Interpreter.Static.Generics where

import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.Map                  as Map

import           Interpreter.Common.Types
import           Parser.Tidy.Abs

import           Interpreter.Common.Errors


bindGenericParameters :: String -> [ClassType] -> [ClassType] -> StaticCheckMonad (Map.Map ObjectType ObjectType)
bindGenericParameters context genericParams genericArgs = do
    when (length genericParams /= length genericArgs) $ throwError $
        GenericArgumentListInvalidError context (show genericParams) (show genericArgs)
    let genericParamTypes = map ObjectTypeClass genericParams
    let genericArgTypes = map ObjectTypeClass genericArgs
    return $ Map.fromList $ zip genericParamTypes genericArgTypes
