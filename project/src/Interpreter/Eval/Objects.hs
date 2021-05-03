module Interpreter.Eval.Objects where

import           Control.Monad.Reader
import qualified Data.Map                 as Map

import           Interpreter.Common.Types
import           Interpreter.Eval.Classes
import           Parser.Tidy.Abs


newSingleValueObject :: SingleValue -> Value
newSingleValueObject = SingleValueObject

newRegularObject :: ValueType -> [Value] -> StateMonad Value
newRegularObject objectType args = do
    objectEnv <- buildObjectEnv objectType args
    return $ RegularObject objectType objectEnv

pass :: Value
pass = newSingleValueObject VoidValue

-- TODO add initialized attributes
buildObjectEnv :: ValueType -> [Value] -> StateMonad ObjectEnv
buildObjectEnv objectType args = do
    (_, classEnv) <- ask
    let classDecl = classEnv Map.! classIdentFromType objectType
    let ctorArgsList = getCtorArgsList classDecl
    let attributes = Map.fromList $ zip ctorArgsList args
    objectValueList <- getValueList objectType
    let (values, variables) = Map.partitionWithKey (\name _ -> name `elem` objectValueList) attributes
    return $ ObjectEnv values variables

argsToExprList :: ArgumentList -> [Expr]
argsToExprList ArgListAbsent = []
argsToExprList (ArgListPresent args) = map argToExpr args

argToExpr :: FunctionArgument -> Expr
argToExpr (FunctionArg expr) = expr

