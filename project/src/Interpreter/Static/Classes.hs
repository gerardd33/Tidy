module Interpreter.Static.Classes where

import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.List                            as List

import           Interpreter.Common.Types
import           Parser.Tidy.Abs

import           Interpreter.Common.Errors
import           Interpreter.Common.Utils.Classes
import           Interpreter.Common.Utils.Expressions
import           Interpreter.Common.Utils.Objects
import           Interpreter.Common.Utils.Types
import           Interpreter.Static.Environments
import           Interpreter.Static.Expressions
import           Interpreter.Static.Methods
import           Interpreter.Static.Types


checkClasses :: [ClassDecl] -> StaticCheckMonad ObjectType
checkClasses declarations = mapM_ checkClass declarations >> returnVoid

checkClass :: ClassDecl -> StaticCheckMonad ObjectType
checkClass classDecl = do
    (_, env) <- registerEmptyClassesInEnv $ getGenericParameterList classDecl
    let methodNames = map methodToObjectIdentifier $ methodNamesFromDeclaration classDecl
    let memberNames = methodNames ++ attributeNamesFromDeclaration classDecl
    assertNoDeclarationRepetitions (showContext $ getClassType classDecl) memberNames
    local (const env) $ checkSections classDecl

checkSections :: ClassDecl -> StaticCheckMonad ObjectType
checkSections (ClassDeclaration _ classTypeModifier classType _ classBody) = do
    case classTypeModifier of
        MMutable -> returnVoid
        MSingleton -> assertVariablesAbsent classType classTypeModifier classBody
        MImmutable -> assertVariablesAbsent classType classTypeModifier classBody >>
            assertActionsAbsent classType classTypeModifier classBody
    checkClassBody classType classTypeModifier classBody

checkClassBody :: ClassType -> ClassTypeModifier -> ClassBody -> StaticCheckMonad ObjectType
checkClassBody _ _ ClassBodyEmpty = returnVoid
checkClassBody classType classTypeModifier (ClassBodyFilled values variables functions actions) = do
    checkAttributeRepetitions (showContext $ classIdentifierFromClassType classType) classType
    checkValuesSection (if classTypeModifier == MSingleton then InitializedRequired else NoneRequired) values
    checkVariablesSection NoneRequired variables
    checkFunctionsSection classType functions
    checkActionsSection classType actions

checkValuesSection :: InitializationType -> ValuesSection -> StaticCheckMonad StaticResult
checkValuesSection _ ValuesAbsent = liftPureStatic returnVoid
checkValuesSection initializationType (ValuesPresent declarations) = do
    checkPureClassAttributeDeclarations "values" declarations
    checkObjectDeclarations initializationType False declarations

checkVariablesSection :: InitializationType -> VariablesSection -> StaticCheckMonad StaticResult
checkVariablesSection _ VariablesAbsent = liftPureStatic returnVoid
checkVariablesSection initializationType (VariablesPresent declarations) = do
    checkPureClassAttributeDeclarations "variables" declarations
    checkObjectDeclarations initializationType True declarations

checkFunctionsSection :: ClassType -> FunctionsSection -> StaticCheckMonad ObjectType
checkFunctionsSection _ FunctionsAbsent = returnVoid
checkFunctionsSection classType (FunctionsPresent declarations) = do
    mapM_ (checkFunctionDeclaration classType) declarations >> returnVoid

checkActionsSection :: ClassType -> ActionsSection -> StaticCheckMonad ObjectType
checkActionsSection _ ActionsAbsent = returnVoid
checkActionsSection classType (ActionsPresent declarations) = do
    mapM_ (checkActionDeclaration classType) declarations >> returnVoid

assertVariablesAbsent :: ClassType -> ClassTypeModifier -> ClassBody -> StaticCheckMonad ObjectType
assertVariablesAbsent _ _ ClassBodyEmpty                          = returnVoid
assertVariablesAbsent _ _ (ClassBodyFilled _ VariablesAbsent _ _) = returnVoid
assertVariablesAbsent classType classTypeModifier _                      =
    throwError $ IllegalSectionError (tail $ show classTypeModifier) (show classType) "variables"

assertActionsAbsent :: ClassType -> ClassTypeModifier -> ClassBody -> StaticCheckMonad ObjectType
assertActionsAbsent _ _ ClassBodyEmpty                          = returnVoid
assertActionsAbsent _ _ (ClassBodyFilled _ _ _ ActionsAbsent)   = returnVoid
assertActionsAbsent classType classTypeModifier _                      =
    throwError $ IllegalSectionError (tail $ show classTypeModifier) (show classType) "actions"

checkPureClassAttributeDeclarations :: String -> [ObjectDecl] -> StaticCheckMonad ObjectType
checkPureClassAttributeDeclarations context declarations = do
    let declarationExpressions = map (snd . objectToNameExprPair) declarations
    case List.find (not . isExpressionPure) declarationExpressions of
        Just nonPureExpression -> throwError $ IllegalSideEffectsError context (showContext nonPureExpression)
        Nothing -> returnVoid
