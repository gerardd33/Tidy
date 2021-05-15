module Interpreter.Static.Objects where

import           Control.Monad.Except

import           Interpreter.Common.Types
import           Parser.Tidy.Abs

import           Interpreter.Common.Errors
import           Interpreter.Common.Utils.Objects
import           Interpreter.Static.Expressions
import           Interpreter.Static.Types


checkObjectDeclaration :: Bool -> ObjectDecl -> StaticCheckMonad ObjectType
checkObjectDeclaration shouldInitialize (ObjectDeclaration _ objectDeclProper) = do
    case objectDeclProper of
        ObjectDeclarationProper objectIdent objectType initialization -> case initialization of
             Uninitialized -> when shouldInitialize (throwError $ UninitializedError $ show objectIdent) >> returnVoid
             Initialized expr -> checkExpression expr >>= assertTypesMatch (showContext objectDeclProper) objectType
