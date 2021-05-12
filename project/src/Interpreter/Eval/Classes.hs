module Interpreter.Eval.Classes where

import           Control.Monad.Reader
import qualified Data.List                         as List
import qualified Data.Map                          as Map
import           Data.Maybe

import           Interpreter.Common.Types
import           Parser.Tidy.Abs

import           Interpreter.Common.Helper.Classes
import           Interpreter.Common.Helper.Methods
import           Interpreter.Common.Helper.Types


getMemberFunction :: ObjectType -> MethodIdent -> StateMonad FunctionDecl
getMemberFunction (ObjectTypeClass classIdent _) functionIdent = do
    (_, classEnv) <- ask
    let functions = getFunctionDecls $ classEnv Map.! classIdent
    return $ fromJust $ List.find (\f -> getFunctionIdentifier f == functionIdent) functions

hasGetter :: ObjectType -> MethodIdent -> StateMonad Bool
hasGetter objectType functionIdent = do
    (_, classEnv) <- ask
    let classDecl = classEnv Map.! classFromObjectType objectType
    let attributeIdentifier = methodToObjectIdentifier functionIdent
    let attributes = getValues classDecl ++ getVariables classDecl
    return $ attributeIdentifier `elem` attributes
