module Interpreter.Static.Environments where

import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.Map                              as Map
import           Data.Maybe

import           Interpreter.Common.Types
import           Parser.Tidy.Abs

import           Interpreter.Common.Errors
import           Interpreter.Common.Utils.Builtin
import           Interpreter.Common.Utils.Classes
import           Interpreter.Common.Utils.Environments
import           Interpreter.Common.Utils.Objects
import           Interpreter.Static.Types


getClassDeclStatic :: ClassIdent -> StaticCheckMonad (Maybe ClassDecl)
getClassDeclStatic classIdent = do
    (_, classEnv) <- ask
    return $ Map.lookup classIdent classEnv

checkObjectType :: ObjectType -> StaticCheckMonad ObjectType
checkObjectType objectType = do
    (_, classEnv) <- ask
    let classIdent = classFromObjectType objectType
    classDecl <- getClassDeclStatic classIdent
    when (isNothing classDecl) $ throwError $ ClassNotInScopeError $ showContext classIdent
    returnVoid

registerLocalObjectType :: ObjectIdent -> ObjectType -> StaticCheckMonad StaticResult
registerLocalObjectType objectIdent objectType = do
    (localEnv, classEnv) <- ask
    let newValues = Map.insert objectIdent objectType (valueTypes localEnv)
    let newLocalEnv = StaticLocalEnv newValues (variableTypes localEnv)
    return (voidType, (newLocalEnv, classEnv))

checkLocalObject :: ObjectIdent -> StaticCheckMonad ObjectType
checkLocalObject objectIdent = do
    (localEnv, _) <- ask
    let localObjectTypes = valueTypes localEnv `Map.union` variableTypes localEnv
    let lookup = Map.lookup objectIdent localObjectTypes
    case lookup of Just objectType -> return objectType
                   Nothing         -> throwError $ ObjectNotInScopeError $ showContext objectIdent
