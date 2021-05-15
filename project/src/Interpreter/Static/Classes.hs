module Interpreter.Static.Classes where

import           Control.Monad.Except

import           Interpreter.Common.Types
import           Parser.Tidy.Abs

import           Interpreter.Common.Errors
import           Interpreter.Static.Types


checkClasses :: [ClassDecl] -> StaticCheckMonad StaticCheckEnv
checkClasses classDeclarations = mapM_ checkClass classDeclarations >> returnChecked

checkClass :: ClassDecl -> StaticCheckMonad StaticCheckEnv
checkClass classDecl = do
    checkProperSections classDecl

checkProperSections :: ClassDecl -> StaticCheckMonad StaticCheckEnv
checkProperSections (ClassDeclaration _ classType _ _ classBody) = do
    case classType of
        MMutable -> returnChecked
        MSingleton -> assertVariablesAbsent classType classBody
        MImmutable -> assertVariablesAbsent classType classBody >> assertActionsAbsent classType classBody

assertVariablesAbsent :: ClassTypeModifier -> ClassBody -> StaticCheckMonad StaticCheckEnv
assertVariablesAbsent _ ClassBodyEmpty                          = returnChecked
assertVariablesAbsent _ (ClassBodyFilled _ VariablesAbsent _ _) = returnChecked
assertVariablesAbsent classType _                               =
    throwError $ ForbiddenSectionError (tail $ show classType) "variables"

assertActionsAbsent :: ClassTypeModifier -> ClassBody -> StaticCheckMonad StaticCheckEnv
assertActionsAbsent _ ClassBodyEmpty                          = returnChecked
assertActionsAbsent _ (ClassBodyFilled _ _ _ ActionsAbsent) = returnChecked
assertActionsAbsent classType _                               =
    throwError $ ForbiddenSectionError (tail $ show classType) "actions"
