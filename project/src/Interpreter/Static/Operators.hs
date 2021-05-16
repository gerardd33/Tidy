module Interpreter.Static.Operators where

import           Interpreter.Common.Types
import           Parser.Tidy.Abs

import           Interpreter.Common.Utils.Builtin
import           Interpreter.Common.Utils.Objects
import           Interpreter.Static.Types


checkLiteral :: Literal -> StaticCheckMonad ObjectType
checkLiteral (LInt int)       = return intType
checkLiteral (LBool bool)     = return boolType
checkLiteral (LChar char)     = return charType
checkLiteral (LString string) = return stringType
checkLiteral (LVoid void)     = return voidType

checkAddition :: String -> ObjectType -> ObjectType -> Expr -> Expr -> StaticCheckMonad ObjectType
checkAddition context type1 type2 expr1 expr2 = do
    assertTypesMatch (showComplexContext expr1 context) type1 intType
    assertTypesMatch (showComplexContext expr2 context) type2 intType
    return intType
