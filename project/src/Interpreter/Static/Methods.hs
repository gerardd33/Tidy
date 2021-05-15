module Interpreter.Static.Methods where

import           Control.Monad.Except
import           Data.List                        as List

import           Interpreter.Common.Types
import           Parser.Tidy.Abs

import           Interpreter.Common.Errors
import           Interpreter.Common.Utils.Objects
import           Interpreter.Static.Expressions
import           Interpreter.Static.Types


assertNoDeclarationRepetitions :: [MethodIdent] -> String -> StaticCheckMonad ObjectType
assertNoDeclarationRepetitions idents context = do
    let duplicates = idents List.\\ nub idents
    unless (null duplicates) $ throwError $ DuplicateDeclarationError
        (showContext $ head duplicates) context
    returnVoid

checkFunctionDeclaration :: FunctionDecl -> StaticCheckMonad ObjectType
checkFunctionDeclaration (FunctionDeclaration _ _ functionIdent functionType functionBody) = do
    returnVoid
--     case objectDeclProper of
--         ObjectDeclarationProper objectIdent objectType initialization -> case initialization of
--              Uninitialized -> when shouldInitialize (throwError $ UninitializedError $ show objectIdent) >> returnVoid
--              Initialized expr -> checkExpression expr >>= assertTypesMatch (showContext objectDeclProper) objectType
