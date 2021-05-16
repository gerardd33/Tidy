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
    checkCommutativeBinaryOperator context intType intType type1 type2 expr1 expr2

-- TODO later add lists
checkConcatenation :: String -> ObjectType -> ObjectType -> Expr -> Expr -> StaticCheckMonad ObjectType
checkConcatenation context type1 type2 expr1 expr2 = do
    checkCommutativeBinaryOperator context stringType stringType type1 type2 expr1 expr2

checkBooleanOperator :: String -> ObjectType -> ObjectType -> Expr -> Expr -> StaticCheckMonad ObjectType
checkBooleanOperator context type1 type2 expr1 expr2 = do
    checkCommutativeBinaryOperator context boolType boolType type1 type2 expr1 expr2

checkRelationalOperator :: String -> ObjectType -> ObjectType -> Expr -> Expr -> StaticCheckMonad ObjectType
checkRelationalOperator context type1 type2 expr1 expr2 = do
    checkCommutativeBinaryOperator context intType boolType type1 type2 expr1 expr2

checkCommutativeBinaryOperator :: String -> ObjectType -> ObjectType ->
    ObjectType -> ObjectType -> Expr -> Expr -> StaticCheckMonad ObjectType
checkCommutativeBinaryOperator context inputType outputType type1 type2 expr1 expr2 = do
    assertTypesMatch (showComplexContext expr1 context) inputType type1
    assertTypesMatch (showComplexContext expr2 context) inputType type2
    return outputType
