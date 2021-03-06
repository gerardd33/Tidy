module Interpreter.Common.Utils.Expressions where

import           Interpreter.Common.Types
import           Parser.Tidy.Abs

import           Interpreter.Common.Errors
{-# ANN module ("HLint: ignore Use record patterns"::String) #-}


isExpressionPure :: Expr -> Bool
isExpressionPure (ELiteral _)                = True
isExpressionPure (ELocalValue _)             = True
isExpressionPure (EGetExpression _)          = True
isExpressionPure (EConstructorCall _)        = True
isExpressionPure (ELambdaFunction _)         = True
isExpressionPure (ELambdaAction _)           = True
isExpressionPure (EFunctionalControlFlow _)  = True
isExpressionPure (EUnaryNot _)               = True
isExpressionPure (EUnaryMinus _)             = True
isExpressionPure (EMultiply _ _)             = True
isExpressionPure (EDivide _ _)               = True
isExpressionPure (EModulo _ _)               = True
isExpressionPure (EAdd _ _)                  = True
isExpressionPure (ESubtract _ _)             = True
isExpressionPure (EConcatenate _ _)          = True
isExpressionPure (ERelationalOperator _ _ _) = True
isExpressionPure (EBooleanOperator _ _ _)    = True

isExpressionPure (EDoExpression _)           = False
isExpressionPure (ELocalDeclaration _)       = False
isExpressionPure (EImperativeControlFlow _)  = False

getThenBranchExpression :: ThenBranch -> Expr
getThenBranchExpression (FThenOneLine expr)   = expr
getThenBranchExpression (FThenMultiLine expr) = expr

showComplexContext :: Expr -> String -> String
showComplexContext expr largerContext = showContext expr ++ "\nIn: " ++ largerContext
