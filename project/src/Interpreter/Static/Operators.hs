module Interpreter.Static.Operators where

import           Interpreter.Common.Types
import           Parser.Tidy.Abs

import           Interpreter.Common.Utils.Builtin


checkLiteral :: Literal -> StaticCheckMonad ObjectType
checkLiteral (LInt int)       = return intType
checkLiteral (LBool bool)     = return boolType
checkLiteral (LChar char)     = return charType
checkLiteral (LString string) = return stringType
checkLiteral (LVoid void)     = return voidType
