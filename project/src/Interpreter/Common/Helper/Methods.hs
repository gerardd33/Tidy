module Interpreter.Common.Helper.Methods where

import           Interpreter.Common.Types
import           Parser.Tidy.Abs

import           Interpreter.Common.Helper.Objects


getFunctionBody :: FunctionDecl -> FunctionBody
getFunctionBody (FunctionDeclaration _ _ _ _ functionBody) = functionBody

getActionBody :: ActionDecl -> ActionBody
getActionBody (ActionDeclaration _ _ _ _ actionBody) = actionBody

getFunctionIdentifier :: FunctionDecl -> MethodIdent
getFunctionIdentifier (FunctionDeclaration _ _ functionIdent _ _) = functionIdent









argsToExprList :: ArgList -> [Expr]
argsToExprList ArgumentListAbsent         = []
argsToExprList (ArgumentListPresent args) = map argToExpr args

argToExpr :: FunctionArg -> Expr
argToExpr (FunctionArgument expr) = expr

getFunctionType :: FunctionDecl -> MethodType
getFunctionType (FunctionDeclaration _ _ _ functionType _) = functionType

getMethodParamsList :: MethodType -> [ObjectIdent]
getMethodParamsList (MethodTypeSignature (ParameterList valueDecls) _) =
    map (getLocalValueName . ObjectDeclaration MPublic) valueDecls

