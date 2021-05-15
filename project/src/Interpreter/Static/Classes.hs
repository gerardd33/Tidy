module Interpreter.Static.Classes where

import           Control.Monad.Except
import           Control.Monad.Reader

import           Interpreter.Common.Types
import           Parser.Tidy.Abs

import           Interpreter.Common.Errors
import           Interpreter.Common.Utils.Classes
import           Interpreter.Common.Utils.Objects
import           Interpreter.Static.Environments
import           Interpreter.Static.Expressions
import           Interpreter.Static.Methods
import           Interpreter.Static.Objects
import           Interpreter.Static.Types


checkClasses :: [ClassDecl] -> StaticCheckMonad ObjectType
checkClasses declarations = mapM_ checkClass declarations >> returnVoid

checkClass :: ClassDecl -> StaticCheckMonad ObjectType
checkClass classDecl = do
    let getterNames = map objectToMethodIdentifier $ attributeNamesFromDeclaration classDecl
    let memberNames =  getterNames ++ methodNamesFromDeclaration classDecl
    assertNoDeclarationRepetitions memberNames $ showContext $ getClassIdentifier classDecl
    checkProperSections classDecl

checkProperSections :: ClassDecl -> StaticCheckMonad ObjectType
checkProperSections (ClassDeclaration _ classType classIdent _ classBody) = do
    case classType of
        MMutable -> returnVoid
        MSingleton -> assertVariablesAbsent classIdent classType classBody
        MImmutable -> assertVariablesAbsent classIdent classType classBody >>
            assertActionsAbsent classIdent classType classBody
    checkClassBody classType classBody

assertVariablesAbsent :: ClassIdent -> ClassTypeModifier -> ClassBody -> StaticCheckMonad ObjectType
assertVariablesAbsent _ _ ClassBodyEmpty                          = returnVoid
assertVariablesAbsent _ _ (ClassBodyFilled _ VariablesAbsent _ _) = returnVoid
assertVariablesAbsent classIdent classType _                      =
    throwError $ ForbiddenSectionError (tail $ show classType) (showContext classIdent) "variables"

assertActionsAbsent :: ClassIdent -> ClassTypeModifier -> ClassBody -> StaticCheckMonad ObjectType
assertActionsAbsent _ _ ClassBodyEmpty                          = returnVoid
assertActionsAbsent _ _ (ClassBodyFilled _ _ _ ActionsAbsent)   = returnVoid
assertActionsAbsent classIdent classType _                      =
    throwError $ ForbiddenSectionError (tail $ show classType) (showContext classIdent) "actions"

checkClassBody :: ClassTypeModifier -> ClassBody -> StaticCheckMonad ObjectType
checkClassBody _ ClassBodyEmpty = returnVoid
checkClassBody classType (ClassBodyFilled values variables functions actions) = do
    checkValuesSection (classType == MSingleton) values
    checkVariablesSection False variables
    checkFunctionsSection functions
--     checkActionsSection actions

checkValuesSection :: Bool -> ValuesSection -> StaticCheckMonad ObjectType
checkValuesSection _ ValuesAbsent = returnVoid
checkValuesSection shouldInitialize (ValuesPresent declarations) = do
    mapM_ (checkObjectDeclaration shouldInitialize) declarations >> returnVoid

checkVariablesSection :: Bool -> VariablesSection -> StaticCheckMonad ObjectType
checkVariablesSection _ VariablesAbsent = returnVoid
checkVariablesSection shouldInitialize (VariablesPresent declarations) = do
    mapM_ (checkObjectDeclaration shouldInitialize) declarations >> returnVoid

checkFunctionsSection :: FunctionsSection -> StaticCheckMonad ObjectType
checkFunctionsSection FunctionsAbsent = returnVoid
checkFunctionsSection (FunctionsPresent declarations) = do
    mapM_ checkFunctionDeclaration declarations >> returnVoid
