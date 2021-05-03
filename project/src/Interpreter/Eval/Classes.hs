module Interpreter.Eval.Classes where

import           Control.Monad.Reader
import qualified Data.List                as List
import qualified Data.Map                 as Map
import           Data.Maybe

import           Interpreter.Common.Types
import           Parser.Tidy.Abs


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

getValueDecls :: ClassDecl -> [ValueDecl]
getValueDecls (ClassDeclConcrete _ _ _ (ClassBodyFilled (ValuesPresent (ValuesSBody valueDecls)) _ _ _)) =
    valueDecls
getValueDecls _ = []

getVariableDecls :: ClassDecl -> [ValueDecl]
getVariableDecls (ClassDeclConcrete _ _ _ (ClassBodyFilled _ (VariablesPresent (VariablesSBody variableDecls)) _ _)) =
    variableDecls
getVariableDecls _ = []

getCtorArgsList :: ClassDecl -> [ValueIdent]
getCtorArgsList classDecl = uninitializedValues ++ uninitializedVariables
    where uninitializedValues = map getValueName $ filter isUninitialized (getValueDecls classDecl)
          uninitializedVariables = map getValueName $ filter isUninitialized (getVariableDecls classDecl)

isUninitialized :: ValueDecl -> Bool
isUninitialized (PublicValueDecl (UninitializedValue _ _))  = True
isUninitialized (PrivateValueDecl (UninitializedValue _ _)) = True
isUninitialized _                                           = False

toNameTypePair :: ValueDecl -> (ValueIdent, ValueType)
toNameTypePair (PublicValueDecl (UninitializedValue valueIdent valueType)) = (valueIdent, valueType)
toNameTypePair (PrivateValueDecl (UninitializedValue valueIdent valueType)) = (valueIdent, valueType)

getValueName :: ValueDecl -> ValueIdent
getValueName (PublicValueDecl (UninitializedValue name _)) = name
getValueName (PublicValueDecl (InitializedValue name _ _)) = name

classIdentFromType :: ValueType -> ClassIdent
classIdentFromType (ValueTypeClass classIdent) = classIdent
