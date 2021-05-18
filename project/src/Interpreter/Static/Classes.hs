module Interpreter.Static.Classes where

import           Control.Monad.Except

import           Interpreter.Common.Types
import           Parser.Tidy.Abs

import           Interpreter.Common.Errors
import           Interpreter.Common.Utils.Classes
import           Interpreter.Common.Utils.Types
import           Interpreter.Static.Expressions
import           Interpreter.Static.Methods
import           Interpreter.Static.Types


checkClasses :: [ClassDecl] -> StaticCheckMonad ObjectType
checkClasses declarations = mapM_ checkClass declarations >> returnVoid

checkClass :: ClassDecl -> StaticCheckMonad ObjectType
checkClass classDecl = do
    let getterNames = map objectToMethodIdentifier $ attributeNamesFromDeclaration classDecl
    let memberNames =  getterNames ++ methodNamesFromDeclaration classDecl
    assertNoDeclarationRepetitions (showContext $ getClassIdentifier classDecl) memberNames
    checkSections classDecl

checkSections :: ClassDecl -> StaticCheckMonad ObjectType
checkSections (ClassDeclaration _ classType classIdent _ classBody) = do
    case classType of
        MMutable -> returnVoid
        MSingleton -> assertVariablesAbsent classIdent classType classBody
        MImmutable -> assertVariablesAbsent classIdent classType classBody >>
            assertActionsAbsent classIdent classType classBody
    checkClassBody classIdent classType classBody

checkClassBody :: ClassIdent -> ClassTypeModifier -> ClassBody -> StaticCheckMonad ObjectType
checkClassBody _ _ ClassBodyEmpty = returnVoid
checkClassBody classIdent classType (ClassBodyFilled values variables functions actions) = do
    checkValuesSection (if classType == MSingleton then InitializedRequired else NoneRequired) values
    checkVariablesSection NoneRequired variables
    checkFunctionsSection classIdent functions
    checkActionsSection classIdent actions

checkValuesSection :: InitializationType -> ValuesSection -> StaticCheckMonad StaticResult
checkValuesSection _ ValuesAbsent = liftPureStatic returnVoid
checkValuesSection initializationType (ValuesPresent declarations) =
    checkObjectDeclarations initializationType False declarations

checkVariablesSection :: InitializationType -> VariablesSection -> StaticCheckMonad StaticResult
checkVariablesSection _ VariablesAbsent = liftPureStatic returnVoid
checkVariablesSection initializationType (VariablesPresent declarations) =
    checkObjectDeclarations initializationType True declarations

checkFunctionsSection :: ClassIdent -> FunctionsSection -> StaticCheckMonad ObjectType
checkFunctionsSection _ FunctionsAbsent = returnVoid
checkFunctionsSection classIdent (FunctionsPresent declarations) = do
    mapM_ (checkFunctionDeclaration classIdent) declarations >> returnVoid

checkActionsSection :: ClassIdent -> ActionsSection -> StaticCheckMonad ObjectType
checkActionsSection _ ActionsAbsent = returnVoid
checkActionsSection classIdent (ActionsPresent declarations) = do
    mapM_ (checkActionDeclaration classIdent) declarations >> returnVoid

assertVariablesAbsent :: ClassIdent -> ClassTypeModifier -> ClassBody -> StaticCheckMonad ObjectType
assertVariablesAbsent _ _ ClassBodyEmpty                          = returnVoid
assertVariablesAbsent _ _ (ClassBodyFilled _ VariablesAbsent _ _) = returnVoid
assertVariablesAbsent classIdent classType _                      =
    throwError $ IllegalSectionError (tail $ show classType) (showContext classIdent) "variables"

assertActionsAbsent :: ClassIdent -> ClassTypeModifier -> ClassBody -> StaticCheckMonad ObjectType
assertActionsAbsent _ _ ClassBodyEmpty                          = returnVoid
assertActionsAbsent _ _ (ClassBodyFilled _ _ _ ActionsAbsent)   = returnVoid
assertActionsAbsent classIdent classType _                      =
    throwError $ IllegalSectionError (tail $ show classType) (showContext classIdent) "actions"
