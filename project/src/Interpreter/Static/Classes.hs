module Interpreter.Static.Classes where

import           Control.Monad.Except
import           Control.Monad.Reader

import           Interpreter.Common.Types
import           Parser.Tidy.Abs

import           Interpreter.Common.Errors
import           Interpreter.Common.Utils.Objects
import           Interpreter.Static.Environments
import           Interpreter.Static.Types


checkClasses :: [ClassDecl] -> StaticCheckMonad ObjectType
checkClasses declarations = mapM_ checkClass declarations >> returnVoid

checkClass :: ClassDecl -> StaticCheckMonad ObjectType
checkClass classDecl = do
    checkProperSections classDecl
    -- TODO more verification later

checkProperSections :: ClassDecl -> StaticCheckMonad ObjectType
checkProperSections (ClassDeclaration _ classType _ _ classBody) = do
    case classType of
        MMutable -> returnVoid
        MSingleton -> assertVariablesAbsent classType classBody
        MImmutable -> assertVariablesAbsent classType classBody >> assertActionsAbsent classType classBody
    checkClassBody classType classBody

assertVariablesAbsent :: ClassTypeModifier -> ClassBody -> StaticCheckMonad ObjectType
assertVariablesAbsent _ ClassBodyEmpty                          = returnVoid
assertVariablesAbsent _ (ClassBodyFilled _ VariablesAbsent _ _) = returnVoid
assertVariablesAbsent classType _                               =
    throwError $ ForbiddenSectionError (tail $ show classType) "variables"

assertActionsAbsent :: ClassTypeModifier -> ClassBody -> StaticCheckMonad ObjectType
assertActionsAbsent _ ClassBodyEmpty                          = returnVoid
assertActionsAbsent _ (ClassBodyFilled _ _ _ ActionsAbsent) = returnVoid
assertActionsAbsent classType _                               =
    throwError $ ForbiddenSectionError (tail $ show classType) "actions"

checkClassBody :: ClassTypeModifier -> ClassBody -> StaticCheckMonad ObjectType
checkClassBody _ ClassBodyEmpty = returnVoid
checkClassBody classType (ClassBodyFilled values variables functions actions) = do
    checkValuesSection (classType == MSingleton) values
--     checkVariablesSection variables
--     checkFunctionsSections functions
--     checkActionsSection actions

checkValuesSection :: Bool -> ValuesSection -> StaticCheckMonad ObjectType
checkValuesSection _ ValuesAbsent = returnVoid
checkValuesSection shouldInitialize (ValuesPresent declarations) =
    mapM_ (checkValueDeclaration shouldInitialize) declarations >> returnVoid

checkValueDeclaration :: Bool -> ObjectDecl -> StaticCheckMonad ObjectType
checkValueDeclaration shouldInitialize (ObjectDeclaration _ objectDeclProper) = do
    case objectDeclProper of
        ObjectDeclarationProper objectIdent objectType initialization -> case initialization of
             Uninitialized -> when shouldInitialize (throwError (UninitializedError (show objectIdent))) >> returnVoid
             Initialized expr -> returnVoid -- TODO checks
