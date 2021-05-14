module Interpreter.Common.Utils.Methods where

import           Interpreter.Common.Types
import           Parser.Tidy.Abs

import           Interpreter.Common.Utils.Objects


getFunctionBody :: FunctionDecl -> FunctionBody
getFunctionBody (FunctionDeclaration _ _ _ _ functionBody) = functionBody

getActionBody :: ActionDecl -> ActionBody
getActionBody (ActionDeclaration _ _ _ _ actionBody) = actionBody

getFunctionIdentifier :: FunctionDecl -> MethodIdent
getFunctionIdentifier (FunctionDeclaration _ _ functionIdent _ _) = functionIdent

getActionIdentifier :: ActionDecl -> MethodIdent
getActionIdentifier (ActionDeclaration _ _ actionIdent _ _) = actionIdent

getFunctionType :: FunctionDecl -> MethodType
getFunctionType (FunctionDeclaration _ _ _ functionType _) = functionType

getActionType :: ActionDecl -> MethodType
getActionType (ActionDeclaration _ _ _ actionType _) = actionType

getMethodParamList :: MethodType -> [ObjectIdent]
getMethodParamList (MethodTypeSignature (ParameterList valueDeclarations) _) =
    map (objectNameFromDeclaration . ObjectDeclaration MPublic) valueDeclarations

isActionMain :: ActionDecl -> Bool
isActionMain actionDecl = getActionIdentifier actionDecl == MethodIdentifier (LowerCaseIdent "main")

argsToExpressionList :: ArgList -> [Expr]
argsToExpressionList ArgumentListAbsent         = []
argsToExpressionList (ArgumentListPresent args) = map argToExpression args

argToExpression :: MethodArg -> Expr
argToExpression (MethodArgument expr) = expr
