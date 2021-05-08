module Interpreter.Eval.Classes where

import           Control.Monad.Reader
import qualified Data.List                          as List
import qualified Data.Map                           as Map
import           Data.Maybe

import           Interpreter.Common.Types
import           Interpreter.Eval.Functions
import           Interpreter.Eval.ValueDeclarations
import           Parser.Tidy.Abs

{-# ANN module ("HLint: ignore Use record patterns"::String) #-}


hasMainAction :: ClassDecl -> Bool
hasMainAction = isJust . getMainAction

-- TODO does not support inherited main method
getMainAction :: ClassDecl -> Maybe ActionDecl
getMainAction (ClassDeclConcrete MSingleton _ _ (ClassBodyFilled _ _ _ (ActionsPresent (ASBodyFilled actions)))) =
    List.find isActionMain actions
getMainAction _ = Nothing

loadClasses :: [ClassDecl] -> ClassEnv
loadClasses declarations = Map.fromList $ map evalClassDeclaration declarations

-- TODO static verification of many things in evaluation functions, e.g. if class has only allowed sections
evalClassDeclaration :: ClassDecl -> (ClassIdent, ClassDecl)
evalClassDeclaration decl = case decl of
    (ClassDeclConcrete _ identifier _ _) -> (identifier, decl)
    (ClassDeclAbstract _ identifier _ _) -> (identifier, decl)

-- TODO here print NoMainActionError and terminate if list empty, once I have monads adapted for static checking
findMainClass :: [ClassDecl] -> ClassDecl
findMainClass = head . filter hasMainAction

isActionMain :: ActionDecl -> Bool
isActionMain (PublicActionDecl (FIdent (LowerCaseIdent "main")) _ _)   = True
isActionMain (OverrideActionDecl (FIdent (LowerCaseIdent "main")) _ _) = True
isActionMain _                                                         = False

getValueList :: ValueType -> StateMonad [ValueIdent]
getValueList (ValueTypeClass className) = do
    (_, classEnv) <- ask
    return $ getValues $ classEnv Map.! className

getValues :: ClassDecl -> [ValueIdent]
getValues classDecl = map getValueName (getValueDecls classDecl)

getVariables :: ClassDecl -> [ValueIdent]
getVariables classDecl = map getValueName (getVariableDecls classDecl)

getValueDecls :: ClassDecl -> [ValueDecl]
getValueDecls (ClassDeclConcrete _ _ _ (ClassBodyFilled (ValuesPresent (ValuesSBody valueDecls)) _ _ _)) =
    valueDecls
getValueDecls _ = []

getVariableDecls :: ClassDecl -> [ValueDecl]
getVariableDecls (ClassDeclConcrete _ _ _ (ClassBodyFilled _ (VariablesPresent (VariablesSBody variableDecls)) _ _)) =
    variableDecls
getVariableDecls _ = []

getCtorParamsList :: ClassDecl -> [ValueIdent]
getCtorParamsList classDecl = uninitializedValues ++ uninitializedVariables
    where uninitializedValues = map getValueName $ filter (not . isInitialized) (getValueDecls classDecl)
          uninitializedVariables = map getValueName $ filter (not . isInitialized) (getVariableDecls classDecl)

classIdentFromType :: ValueType -> ClassIdent
classIdentFromType (ValueTypeClass classIdent) = classIdent

getInitializedAttributeList :: ClassDecl -> [(ValueIdent, Expr)]
getInitializedAttributeList classDecl = initializedValues ++ initializedVariables
    where initializedValues = map getNameExprPair $ filter isInitialized (getValueDecls classDecl)
          initializedVariables = map getNameExprPair $ filter isInitialized (getVariableDecls classDecl)

getMemberFunction :: ValueType -> FunctionIdent -> StateMonad FunctionDecl
getMemberFunction (ValueTypeClass className) functionIdentifier = do
    (_, classEnv) <- ask
    let functions = getFunctionDecls $ classEnv Map.! className
    return $ fromJust $ List.find (\f -> getFunctionName f == functionIdentifier) functions

getFunctionDecls :: ClassDecl -> [FunctionDecl]
getFunctionDecls (ClassDeclConcrete _ _ _ (ClassBodyFilled _ _ (FunctionsPresent (FSBodyFilled functionDecls)) _)) =
    functionDecls
getFunctionDecls _ = []

hasGetter :: ValueType -> FunctionIdent -> StateMonad Bool
hasGetter objectType functionIdentifier = do
    (_, classEnv) <- ask
    let classDecl = classEnv Map.! classIdentFromType objectType
    let attributeIdentifier = functionToValueIdent functionIdentifier
    let attributes = getValues classDecl ++ getVariables classDecl
    return $ attributeIdentifier `elem` attributes
