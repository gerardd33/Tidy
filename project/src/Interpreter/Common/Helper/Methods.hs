module Interpreter.Common.Helper.Methods where

import           Interpreter.Common.Types
import           Parser.Tidy.Abs

import           Interpreter.Common.Helper.Objects


getActionBody :: ActionDecl -> ActionBody
getActionBody (ActionDeclaration _ _ _ _ actionBody) = actionBody









argsToExprList :: ArgList -> [Expr]
argsToExprList ArgumentListAbsent         = []
argsToExprList (ArgumentListPresent args) = map argToExpr args

argToExpr :: FunctionArg -> Expr
argToExpr (FunctionArgument expr) = expr

getFunctionName :: FunctionDecl -> MethodIdent
getFunctionName (FunctionDeclaration _ _ functionName _ _) = functionName

getFunctionType :: FunctionDecl -> MethodType
getFunctionType (FunctionDeclaration _ _ _ functionType _) = functionType

getMethodParamsList :: MethodType -> [ObjectIdent]
getMethodParamsList (MethodTypeSignature (ParameterList valueDecls) _) =
    map (getLocalValueName . ObjectDeclaration MPublic) valueDecls

functionToObjectIdent :: MethodIdent -> ObjectIdent
functionToObjectIdent (MethodIdentifier ident) = ObjectIdentifier ident
