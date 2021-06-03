module Interpreter.Runtime.Operators where

import           Control.Monad.Except

import           Interpreter.Common.Types
import           Parser.Tidy.Abs

import           Interpreter.Common.Errors
import           Interpreter.Common.Utils.Types
import           Interpreter.Runtime.Environments


evaluateLiteral :: Literal -> StateMonad Object
evaluateLiteral (LInt int)       = return $ BuiltinObject $ IntObject int
evaluateLiteral (LBool bool)     = return $ BuiltinObject $ BoolObject bool
evaluateLiteral (LChar char)     = return $ BuiltinObject $ CharObject char
evaluateLiteral (LString string) = return $ BuiltinObject $ StringObject string
evaluateLiteral (LVoid void)     = return $ BuiltinObject VoidObject

evaluateAddition :: Object -> Object -> Expr -> Expr -> StateMonad Object
evaluateAddition (BuiltinObject (IntObject value1)) (BuiltinObject (IntObject value2)) _ _ =
    return $ BuiltinObject $ IntObject $ value1 + value2

evaluateSubtraction :: Object -> Object -> Expr -> Expr -> StateMonad Object
evaluateSubtraction (BuiltinObject (IntObject value1)) (BuiltinObject (IntObject value2)) _ _ =
    return $ BuiltinObject $ IntObject $ value1 - value2

evaluateMultiplication :: Object -> Object -> Expr -> Expr -> StateMonad Object
evaluateMultiplication (BuiltinObject (IntObject value1)) (BuiltinObject (IntObject value2)) _ _ =
    return $ BuiltinObject $ IntObject $ value1 * value2

evaluateDivision :: Object -> Object -> Expr -> Expr -> StateMonad Object
evaluateDivision (BuiltinObject (IntObject value1)) (BuiltinObject (IntObject value2)) _ expr2 =
    if value2 == 0 then throwError $ DivideByZeroException $ showContext expr2
    else return $ BuiltinObject $ IntObject $ value1 `div` value2

evaluateModulo :: Object -> Object -> Expr -> Expr -> StateMonad Object
evaluateModulo (BuiltinObject (IntObject value1)) (BuiltinObject (IntObject value2)) _ expr2 =
    if value2 == 0 then throwError $ DivideByZeroException $ showContext expr2
    else return $ BuiltinObject $ IntObject $ value1 `mod` value2

evaluateConcatenation :: Object -> Object -> Expr -> Expr -> StateMonad Object
evaluateConcatenation (BuiltinObject (StringObject value1)) (BuiltinObject (StringObject value2)) _ _ =
    return $ BuiltinObject $ StringObject $ value1 ++ value2

evaluateUnaryNot :: Object -> StateMonad Object
evaluateUnaryNot (BuiltinObject (BoolObject BTrue)) = return $ BuiltinObject $ BoolObject BFalse
evaluateUnaryNot (BuiltinObject (BoolObject BFalse)) = return $ BuiltinObject $ BoolObject BTrue

evaluateUnaryMinus :: Object -> StateMonad Object
evaluateUnaryMinus (BuiltinObject (IntObject value)) = return $ BuiltinObject $ IntObject $ -value

evaluateRelational :: (Integer -> Integer -> Bool) -> Object -> Object -> Expr -> Expr -> StateMonad Object
evaluateRelational operator (BuiltinObject (IntObject value1)) (BuiltinObject (IntObject value2)) _ _ =
    return $ BuiltinObject $ BoolObject $ toBoolean $ operator value1 value2

evaluateBooleanAnd :: Object -> Object -> Expr -> Expr -> StateMonad Object
evaluateBooleanAnd (BuiltinObject (BoolObject value1)) (BuiltinObject (BoolObject value2)) _ _ =
    return $ BuiltinObject $ BoolObject $ toBoolean $ fromBoolean value1 && fromBoolean value2

evaluateBooleanOr :: Object -> Object -> Expr -> Expr -> StateMonad Object
evaluateBooleanOr (BuiltinObject (BoolObject value1)) (BuiltinObject (BoolObject value2)) _ _ =
    return $ BuiltinObject $ BoolObject $ toBoolean $ fromBoolean value1 || fromBoolean value2

evaluateEquality :: Object -> Object -> Expr -> Expr -> StateMonad Object
evaluateEquality value1 value2 _ _ = do
    result <- objectsEqual value1 value2
    return $ BuiltinObject $ BoolObject $ toBoolean result

evaluateNonEquality :: Object -> Object -> Expr -> Expr -> StateMonad Object
evaluateNonEquality value1 value2 _ _ = do
    result <- objectsEqual value1 value2
    return $ BuiltinObject $ BoolObject $ toBoolean $ not result
