module Interpreter.Common.Helper.Classes where

import qualified Data.List                         as List
import qualified Data.Map                          as Map
import           Data.Maybe

import           Interpreter.Common.Helper.Objects
import           Interpreter.Common.Types
import           Parser.Tidy.Abs


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

getFunctionDecls :: ClassDecl -> [FunctionDecl]
getFunctionDecls (ClassDeclaration _ _ _ _ (ClassBodyFilled _ _ (FunctionsPresent functionDecls) _)) =
    functionDecls
getFunctionDecls _ = []

isSingletonClass :: ClassDecl -> Bool
isSingletonClass (ClassDeclaration _ MSingleton _ _ _) = True
isSingletonClass _                                     = False

singletonInstanceIdentifier :: ClassIdent -> ObjectIdent
singletonInstanceIdentifier (ClassIdentifier (UpperCaseIdent name)) =
    ObjectIdentifier (LowerCaseIdent ("_singleton_" ++ name))
