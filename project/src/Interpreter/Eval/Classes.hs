module Interpreter.Eval.Classes where

import           Control.Monad.Reader
import qualified Data.List                          as List
import qualified Data.Map                           as Map
import           Data.Maybe

import           Interpreter.Common.Types
import           Interpreter.Eval.Functions
import           Interpreter.Eval.Objects
import           Interpreter.Eval.ValueDeclarations
import           Parser.Tidy.Abs

{-# ANN module ("HLint: ignore Use record patterns"::String) #-}


hasMainAction :: ClassDecl -> Bool
hasMainAction = isJust . getMainAction

getMainAction :: ClassDecl -> Maybe ActionDecl
getMainAction (ClassDeclaration _ _ _ _ (ClassBodyFilled _ _ _ (ActionsPresent actions))) =
    List.find isActionMain actions
getMainAction _ = Nothing

loadClasses :: [ClassDecl] -> ClassEnv
loadClasses declarations = Map.fromList $ map evalClassDeclaration declarations

-- TODO static verification of many things in evaluation functions, e.g. if class has only allowed sections
evalClassDeclaration :: ClassDecl -> (ClassIdent, ClassDecl)
evalClassDeclaration declaration = case declaration of
    (ClassDeclaration _ _ identifier _ _)  -> (identifier, declaration)

-- TODO here print NoMainActionError and terminate if list empty, once I have monads adapted for static checking
findMainClass :: [ClassDecl] -> ClassDecl
findMainClass = head . filter hasMainAction

isActionMain :: ActionDecl -> Bool
isActionMain (ActionDeclaration _ _ (MethodIdentifier (LowerCaseIdent "main")) _ _)   = True
isActionMain _                                                                        = False

getValueList :: ObjectType -> StateMonad [ObjectIdent]
getValueList (ObjectTypeClass className _) = do
    (_, classEnv) <- ask
    return $ getValues $ classEnv Map.! className

getValues :: ClassDecl -> [ObjectIdent]
getValues classDecl = map getObjectName (getValueDecls classDecl)

getVariables :: ClassDecl -> [ObjectIdent]
getVariables classDecl = map getObjectName (getVariableDecls classDecl)

getValueDecls :: ClassDecl -> [ObjectDecl]
getValueDecls (ClassDeclaration _ _ _ _ (ClassBodyFilled (ValuesPresent valueDecls) _ _ _)) =
    valueDecls
getValueDecls _ = []

getVariableDecls :: ClassDecl -> [ObjectDecl]
getVariableDecls (ClassDeclaration _ _ _ _ (ClassBodyFilled _ (VariablesPresent variableDecls) _ _)) =
    variableDecls
getVariableDecls _ = []

getCtorParamsList :: ClassDecl -> [ObjectIdent]
getCtorParamsList classDecl = uninitializedValues ++ uninitializedVariables
    where uninitializedValues = map getObjectName $ filter (not . isInitialized) (getValueDecls classDecl)
          uninitializedVariables = map getObjectName $ filter (not . isInitialized) (getVariableDecls classDecl)

classIdentFromType :: ObjectType -> ClassIdent
classIdentFromType (ObjectTypeClass classIdent _) = classIdent

getInitializedAttributeList :: ClassDecl -> [(ObjectIdent, Expr)]
getInitializedAttributeList classDecl = initializedValues ++ initializedVariables
    where initializedValues = map toNameExprPair $ filter isInitialized (getValueDecls classDecl)
          initializedVariables = map toNameExprPair $ filter isInitialized (getVariableDecls classDecl)

getMemberFunction :: ObjectType -> MethodIdent -> StateMonad FunctionDecl
getMemberFunction (ObjectTypeClass className _) functionIdentifier = do
    (_, classEnv) <- ask
    let functions = getFunctionDecls $ classEnv Map.! className
    return $ fromJust $ List.find (\f -> getFunctionName f == functionIdentifier) functions

getFunctionDecls :: ClassDecl -> [FunctionDecl]
getFunctionDecls (ClassDeclaration _ _ _ _ (ClassBodyFilled _ _ (FunctionsPresent functionDecls) _)) =
    functionDecls
getFunctionDecls _ = []

hasGetter :: ObjectType -> MethodIdent -> StateMonad Bool
hasGetter objectType functionIdentifier = do
    (_, classEnv) <- ask
    let classDecl = classEnv Map.! classIdentFromType objectType
    let attributeIdentifier = functionToObjectIdent functionIdentifier
    let attributes = getValues classDecl ++ getVariables classDecl
    return $ attributeIdentifier `elem` attributes

singletonInstanceIdentifier :: ClassIdent -> ObjectIdent
singletonInstanceIdentifier (ClassIdentifier (UpperCaseIdent name)) =
    ObjectIdentifier (LowerCaseIdent ("_singleton_" ++ name))

buildObjectEnv :: ObjectType -> [Value] -> [(ObjectIdent, Value)] -> StateMonad ObjectEnv
buildObjectEnv objectType args initializedAttributes = do
    (_, classEnv) <- ask
    let classDecl = classEnv Map.! classIdentFromType objectType
    let ctorParamsList = getCtorParamsList classDecl
    let attributesFromCtor = Map.fromList $ zip ctorParamsList args
    let attributes = Map.union (Map.fromList initializedAttributes) attributesFromCtor
    objectValueList <- getValueList objectType
    let (values, variables) = Map.partitionWithKey (\name _ -> name `elem` objectValueList) attributes
    return $ ObjectEnv values variables

isSingletonClass :: ClassDecl -> Bool
isSingletonClass (ClassDeclaration _ MSingleton _ _ _) = True
isSingletonClass _                                     = False
