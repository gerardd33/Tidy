module Interpreter.Static.Generics where

import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.Map                         as Map

import           Interpreter.Common.Types
import           Parser.Tidy.Abs

import           Interpreter.Common.Errors
import           Interpreter.Common.Utils.Classes
import           Interpreter.Static.Environments


bindGenericParameters :: String -> [ClassType] -> [ClassType] -> StaticCheckMonad GenericsMap
bindGenericParameters context genericParams genericArgs = do
    when (length genericParams /= length genericArgs) $ throwError $
        GenericArgumentListInvalidError context (show genericParams) (show genericArgs)
    return $ createGenericsMap genericParams genericArgs

genericsMapFromClassType :: ClassType -> StaticCheckMonad GenericsMap
genericsMapFromClassType classType = do
    let classIdent = classIdentifierFromClassType classType
    let genericArgs = genericParameterListFromClassType classType
    classDecl <- getClassDeclarationStatic classType
    let genericParams = getGenericParameterList classDecl
    return $ createGenericsMap genericParams genericArgs

createGenericsMap :: [ClassType] -> [ClassType] -> GenericsMap
createGenericsMap genericParams genericArgs = Map.fromList $ zip genericParamTypes genericArgTypes
    where genericParamTypes = map ObjectTypeClass genericParams
          genericArgTypes = map ObjectTypeClass genericArgs
