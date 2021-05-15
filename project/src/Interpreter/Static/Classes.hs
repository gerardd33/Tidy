module Interpreter.Static.Classes where

import           Control.Monad.Except
import           Control.Monad.Reader

import           Interpreter.Common.Types
import           Parser.Tidy.Abs

import           Interpreter.Common.Errors
import           Interpreter.Static.Types


checkClasses :: [ClassDecl] -> StaticCheckMonad StaticCheckEnv
checkClasses classDeclarations = mapM_ checkClass classDeclarations >> returnSuccessful

checkClass :: ClassDecl -> StaticCheckMonad StaticCheckEnv
checkClass classDecl = do
    checkProperSections classDecl

checkProperSections :: ClassDecl -> StaticCheckMonad StaticCheckEnv
checkProperSections (ClassDeclaration _ classType _ _ classBody) = do
    case classType of
        MMutable -> returnSuccessful
        MSingleton -> assertVariablesAbsent classType classBody
        MImmutable -> assertVariablesAbsent classType classBody >> assertActionsAbsent classType classBody
    checkClassBody classType classBody

assertVariablesAbsent :: ClassTypeModifier -> ClassBody -> StaticCheckMonad StaticCheckEnv
assertVariablesAbsent _ ClassBodyEmpty                          = returnSuccessful
assertVariablesAbsent _ (ClassBodyFilled _ VariablesAbsent _ _) = returnSuccessful
assertVariablesAbsent classType _                               =
    throwError $ ForbiddenSectionError (tail $ show classType) "variables"

assertActionsAbsent :: ClassTypeModifier -> ClassBody -> StaticCheckMonad StaticCheckEnv
assertActionsAbsent _ ClassBodyEmpty                          = returnSuccessful
assertActionsAbsent _ (ClassBodyFilled _ _ _ ActionsAbsent) = returnSuccessful
assertActionsAbsent classType _                               =
    throwError $ ForbiddenSectionError (tail $ show classType) "actions"

checkClassBody :: ClassTypeModifier -> ClassBody -> StaticCheckMonad StaticCheckEnv
checkClassBody _ ClassBodyEmpty = returnSuccessful
checkClassBody classType (ClassBodyFilled values variables functions actions) = do
    checkValuesSection (classType == MSingleton) values
--     checkVariablesSection variables
--     checkFunctionsSections functions
--     checkActionsSection actions

checkValuesSection :: Bool -> ValuesSection -> StaticCheckMonad StaticCheckEnv
checkValuesSection _ ValuesAbsent = returnSuccessful
checkValuesSection shouldInitialize (ValuesPresent declarations) = checkValueDeclarations shouldInitialize declarations

checkValueDeclarations :: Bool -> [ObjectDecl] -> StaticCheckMonad StaticCheckEnv
checkValueDeclarations shouldInitialize [] = returnSuccessful
checkValueDeclarations shouldInitialize (decl:decls) = do
    env <- checkValueDeclaration shouldInitialize decl
    local (const env) $ checkValueDeclarations shouldInitialize decls

checkValueDeclaration :: Bool -> ObjectDecl -> StaticCheckMonad StaticCheckEnv
checkValueDeclaration shouldInitialize (ObjectDeclaration _ objectDeclProper) = do
    case objectDeclProper of
        ObjectDeclarationProper objectIdent objectType initialization -> case initialization of
             Uninitialized -> when shouldInitialize (throwError (UninitializedError (show objectIdent))) >> returnSuccessful
             Initialized expr -> returnSuccessful -- TODO checks
