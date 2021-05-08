module Interpreter.Eval.Functions where

import           Interpreter.Common.Types
import           Interpreter.Eval.ValueDeclarations
import           Parser.Tidy.Abs


argsToExprList :: ArgumentList -> [Expr]
argsToExprList ArgListAbsent         = []
argsToExprList (ArgListPresent args) = map argToExpr args

argToExpr :: FunctionArgument -> Expr
argToExpr (FunctionArg expr) = expr

getFunctionName :: FunctionDecl -> FunctionIdent
getFunctionName (OverrideFunctionDecl identifier _ _) = identifier
getFunctionName (PublicFunctionDecl identifier _ _)   = identifier
getFunctionName (PrivateFunctionDecl identifier _ _)  = identifier

getFunctionType :: FunctionDecl -> MethodType
getFunctionType (OverrideFunctionDecl _ methodType _) = methodType
getFunctionType (PublicFunctionDecl _ methodType _)   = methodType
getFunctionType (PrivateFunctionDecl _ methodType _)  = methodType

getMethodParamsList :: MethodType -> [ValueIdent]
getMethodParamsList (FType (ParamList valueDecls) _) =
    map (getValueName . PublicValueDecl) valueDecls
