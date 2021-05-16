module Interpreter.Common.Utils.Expressions where

import           Interpreter.Common.Types
import           Parser.Tidy.Abs
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
