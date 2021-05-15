module Interpreter.Static.Classes where

import           Control.Monad.Except
import           Control.Monad.Reader

import           Interpreter.Common.Types
import           Parser.Tidy.Abs

import           Interpreter.Common.Errors
import           Interpreter.Static.Types


checkClasses :: [ClassDecl] -> StaticCheckMonad StaticObject
checkClasses classDeclarations = mapM_ checkClass classDeclarations >> returnPureStatic returnPassStatic

checkClass :: ClassDecl -> StaticCheckMonad StaticResult
checkClass classDecl = do
    checkProperSections classDecl

checkProperSections :: ClassDecl -> StaticCheckMonad StaticResult
checkProperSections (ClassDeclaration _ classType _ _ classBody) = do
    case classType of
        MMutable -> returnPassStatic
        MSingleton -> assertVariablesAbsent classType classBody
        MImmutable -> assertVariablesAbsent classType classBody >> assertActionsAbsent classType classBody
    checkClassBody classType classBody

assertVariablesAbsent :: ClassTypeModifier -> ClassBody -> StaticCheckMonad StaticResult
assertVariablesAbsent _ ClassBodyEmpty                          = returnPassStatic
assertVariablesAbsent _ (ClassBodyFilled _ VariablesAbsent _ _) = returnPassStatic
assertVariablesAbsent classType _                               =
    throwError $ ForbiddenSectionError (tail $ show classType) "variables"

assertActionsAbsent :: ClassTypeModifier -> ClassBody -> StaticCheckMonad StaticResult
assertActionsAbsent _ ClassBodyEmpty                          = returnPassStatic
assertActionsAbsent _ (ClassBodyFilled _ _ _ ActionsAbsent) = returnPassStatic
assertActionsAbsent classType _                               =
    throwError $ ForbiddenSectionError (tail $ show classType) "actions"

checkClassBody :: ClassTypeModifier -> ClassBody -> StaticCheckMonad StaticResult
checkClassBody _ ClassBodyEmpty = returnPassStatic
checkClassBody classType (ClassBodyFilled values variables functions actions) = do
    checkValuesSection (classType == MSingleton) values
--     checkVariablesSection variables
--     checkFunctionsSections functions
--     checkActionsSection actions

checkValuesSection :: Bool -> ValuesSection -> StaticCheckMonad StaticResult
checkValuesSection _ ValuesAbsent = returnPassStatic
checkValuesSection shouldInitialize (ValuesPresent declarations) = checkValueDeclarations shouldInitialize declarations

checkValueDeclarations :: Bool -> [ObjectDecl] -> StaticCheckMonad StaticResult
checkValueDeclarations shouldInitialize [] = returnPassStatic
checkValueDeclarations shouldInitialize (decl:decls) = do
    (_, env) <- checkValueDeclaration shouldInitialize decl
    local (const env) $ checkValueDeclarations shouldInitialize decls

checkValueDeclaration :: Bool -> ObjectDecl -> StaticCheckMonad StaticResult
checkValueDeclaration shouldInitialize (ObjectDeclaration _ objectDeclProper) = do
    case objectDeclProper of
        ObjectDeclarationProper objectIdent objectType initialization -> case initialization of
             Uninitialized -> when shouldInitialize (throwError (UninitializedError (show objectIdent))) >> returnPassStatic
             Initialized expr -> returnPassStatic -- TODO checks
