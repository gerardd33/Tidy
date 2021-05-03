module Interpreter.Eval.Classes where

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
    (ClassDeclConcrete modifier identifier inheritance body) -> (identifier, decl)
    (ClassDeclAbstract modifier identifier inheritance body) -> (identifier, decl)

-- TODO here print NoMainActionError and terminate if list empty, once I have monads adapted for static checking
findMainClass :: [ClassDecl] -> ClassDecl
findMainClass = head . filter hasMainAction

isActionMain :: ActionDecl -> Bool
isActionMain (PublicActionDecl (FIdent (LowerCaseIdent "main")) _ _)   = True
isActionMain (OverrideActionDecl (FIdent (LowerCaseIdent "main")) _ _) = True
isActionMain _                                                         = False

-- getValueList :: ValueType ->
