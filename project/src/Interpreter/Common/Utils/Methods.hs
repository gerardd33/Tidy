module Interpreter.Common.Utils.Methods where

import           Interpreter.Common.Types
import           Parser.Tidy.Abs

import           Interpreter.Common.Errors
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

getMethodParamNames :: MethodType -> [ObjectIdent]
getMethodParamNames methodType = map (getObjectIdentifier . publicDeclarationFromProper) declarations
    where declarations = getMethodParamDeclarations methodType

getMethodParamTypes :: MethodType -> [ObjectType]
getMethodParamTypes methodType = map (objectTypeFromDeclaration . publicDeclarationFromProper) declarations
    where declarations = getMethodParamDeclarations methodType

getMethodParamDeclarations :: MethodType -> [ObjectDeclProper]
getMethodParamDeclarations (MethodTypeSignature (ParameterList paramDeclarations) _) = paramDeclarations

getMethodReturnType :: MethodType -> ObjectType
getMethodReturnType (MethodTypeSignature _ returnType) = returnType

getFunctionWithValuesNames :: FunctionBody -> [ObjectIdent]
getFunctionWithValuesNames functionBody = map (getObjectIdentifier . publicDeclarationFromProper) declarations
    where declarations = getFunctionWithValuesDeclarations functionBody

getFunctionWithValuesDeclarations :: FunctionBody -> [ObjectDeclProper]
getFunctionWithValuesDeclarations (FunctionBodyMultiLine _ (WithValuesPresent (ValuesPresent declarations))) =
    map getProperDeclaration declarations
getFunctionWithValuesDeclarations _ = []

isActionMain :: ActionDecl -> Bool
isActionMain actionDecl = getActionIdentifier actionDecl == MethodIdentifier (LowerCaseIdent "main")

argsToExpressionList :: ArgList -> [Expr]
argsToExpressionList ArgumentListAbsent         = []
argsToExpressionList (ArgumentListPresent args) = map argToExpression args

argToExpression :: MethodArg -> Expr
argToExpression (MethodArgument expr) = expr

showMethodContext :: MethodIdent -> MethodType -> String
showMethodContext methodIdent methodType = showContext methodIdent ++ ": " ++ showContext methodType

functionToNameTypePair :: FunctionDecl -> (MethodIdent, MethodType)
functionToNameTypePair (FunctionDeclaration _ _ methodName methodType _) = (methodName, methodType)

actionToNameTypePair :: ActionDecl -> (MethodIdent, MethodType)
actionToNameTypePair (ActionDeclaration _ _ methodName methodType _) = (methodName, methodType)
