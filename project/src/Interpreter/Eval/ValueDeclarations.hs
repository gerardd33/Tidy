module Interpreter.Eval.ValueDeclarations where

import           Control.Monad.Reader
import           Interpreter.Common.Types
import           Parser.Tidy.Abs


isInitialized :: ObjectDecl -> Bool
isInitialized (ObjectDeclaration _ (ObjectDeclarationProper _ _ (Initialized _)))  = True
isInitialized _                                                                    = False

toNameTypePair :: ObjectDecl -> (ObjectIdent, ObjectType)
toNameTypePair (ObjectDeclaration _ (ObjectDeclarationProper objectIdentifier objectType _))
    = (objectIdentifier, objectType)

getObjectName :: ObjectDecl -> ObjectIdent
getObjectName (ObjectDeclaration _ (ObjectDeclarationProper objectIdentifier _ _)) = objectIdentifier

toNameExprPair :: ObjectDecl -> (ObjectIdent, Expr)
toNameExprPair (ObjectDeclaration _ (ObjectDeclarationProper objectIdentifier _ (Initialized expression))) =
    (objectIdentifier, expression)

getProperObjectDecl :: ObjectDecl -> ObjectDeclProper
getProperObjectDecl (ObjectDeclaration _ properDeclaration) = properDeclaration
