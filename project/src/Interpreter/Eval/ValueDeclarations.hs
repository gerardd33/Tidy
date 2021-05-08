module Interpreter.Eval.ValueDeclarations where

import           Control.Monad.Reader
import           Interpreter.Common.Types
import           Interpreter.Eval.Environment
import           Parser.Tidy.Abs


isInitialized :: ValueDecl -> Bool
isInitialized (PublicValueDecl (InitializedValue _ _ _))  = True
isInitialized (PrivateValueDecl (InitializedValue _ _ _)) = True
isInitialized _                                           = False

toNameTypePair :: ValueDecl -> (ValueIdent, ValueType)
toNameTypePair (PublicValueDecl (UninitializedValue valueIdent valueType)) = (valueIdent, valueType)
toNameTypePair (PrivateValueDecl (UninitializedValue valueIdent valueType)) = (valueIdent, valueType)

getValueName :: ValueDecl -> ValueIdent
getValueName (PublicValueDecl (UninitializedValue name _))  = name
getValueName (PublicValueDecl (InitializedValue name _ _))  = name
getValueName (PrivateValueDecl (UninitializedValue name _)) = name
getValueName (PrivateValueDecl (InitializedValue name _ _)) = name

getNameExprPair :: ValueDecl -> (ValueIdent, Expr)
getNameExprPair (PublicValueDecl (InitializedValue name _ expr))  = (name, expr)
getNameExprPair (PrivateValueDecl (InitializedValue name _ expr)) = (name, expr)

getProperValueDecl :: ValueDecl -> ValueDeclProper
getProperValueDecl (PublicValueDecl declProper)  = declProper
getProperValueDecl (PrivateValueDecl declProper) = declProper
