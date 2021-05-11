module Interpreter.Eval.Functions where

import           Interpreter.Common.Types
import           Interpreter.Eval.ValueDeclarations
import           Parser.Tidy.Abs


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
    map (getObjectName . ObjectDeclaration MPublic) valueDecls

functionToObjectIdent :: MethodIdent -> ObjectIdent
functionToObjectIdent (MethodIdentifier ident) = ObjectIdentifier ident
