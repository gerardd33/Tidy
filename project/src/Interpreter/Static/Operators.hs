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

checkIntegerOperator :: String -> ObjectType -> ObjectType -> Expr -> Expr -> StaticCheckMonad ObjectType
checkIntegerOperator context type1 type2 expr1 expr2 = do
    assertTypesMatch (showComplexContext expr1 context) intType type1
    assertTypesMatch (showComplexContext expr2 context) intType type2
    return intType

-- TODO later add lists
checkConcatenation :: String -> ObjectType -> ObjectType -> Expr -> Expr -> StaticCheckMonad ObjectType
checkConcatenation context type1 type2 expr1 expr2 = do
    assertTypesMatch (showComplexContext expr1 context) stringType type1
    assertTypesMatch (showComplexContext expr2 context) stringType type2
    return stringType
