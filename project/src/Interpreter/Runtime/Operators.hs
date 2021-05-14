module Interpreter.Runtime.Operators where

import           Interpreter.Common.Types
import           Parser.Tidy.Abs

import           Interpreter.Common.Utils.Types


evaluateLiteral :: Literal -> StateMonad Object
evaluateLiteral (LInt int)       = return $ BuiltinObject $ IntObject int
evaluateLiteral (LBool bool)     = return $ BuiltinObject $ BoolObject bool
evaluateLiteral (LChar char)     = return $ BuiltinObject $ CharObject char
evaluateLiteral (LString string) = return $ BuiltinObject $ StringObject string
evaluateLiteral (LVoid void)     = return $ BuiltinObject VoidObject

evaluateAddition :: Object -> Object -> StateMonad Object
evaluateAddition (BuiltinObject (IntObject value1)) (BuiltinObject (IntObject value2)) =
    return $ BuiltinObject $ IntObject $ value1 + value2

evaluateSubtraction :: Object -> Object -> StateMonad Object
evaluateSubtraction (BuiltinObject (IntObject value1)) (BuiltinObject (IntObject value2)) =
    return $ BuiltinObject $ IntObject $ value1 - value2

evaluateMultiplication :: Object -> Object -> StateMonad Object
evaluateMultiplication (BuiltinObject (IntObject value1)) (BuiltinObject (IntObject value2)) =
    return $ BuiltinObject $ IntObject $ value1 * value2

-- TODO throw if division by zero
evaluateDivision :: Object -> Object -> StateMonad Object
evaluateDivision (BuiltinObject (IntObject value1)) (BuiltinObject (IntObject value2)) =
    return $ BuiltinObject $ IntObject $ value1 `div` value2

evaluateModulo :: Object -> Object -> StateMonad Object
evaluateModulo (BuiltinObject (IntObject value1)) (BuiltinObject (IntObject value2)) =
    return $ BuiltinObject $ IntObject $ value1 `mod` value2

evaluateConcatenation :: Object -> Object -> StateMonad Object
evaluateConcatenation (BuiltinObject (StringObject value1)) (BuiltinObject (StringObject value2)) =
    return $ BuiltinObject $ StringObject $ value1 ++ value2

evaluateUnaryNot :: Object -> StateMonad Object
evaluateUnaryNot (BuiltinObject (BoolObject BTrue)) = return $ BuiltinObject $ BoolObject BFalse
evaluateUnaryNot (BuiltinObject (BoolObject BFalse)) = return $ BuiltinObject $ BoolObject BTrue

evaluateUnaryMinus :: Object -> StateMonad Object
evaluateUnaryMinus (BuiltinObject (IntObject value)) = return $ BuiltinObject $ IntObject $ -value

evaluateRelational :: (Integer -> Integer -> Bool) -> Object -> Object -> StateMonad Object
evaluateRelational operator (BuiltinObject (IntObject value1)) (BuiltinObject (IntObject value2)) =
    return $ BuiltinObject $ BoolObject $ toBoolean $ operator value1 value2

evaluateBooleanAnd :: Object -> Object -> StateMonad Object
evaluateBooleanAnd (BuiltinObject (BoolObject value1)) (BuiltinObject (BoolObject value2)) =
    return $ BuiltinObject $ BoolObject $ toBoolean $ fromBoolean value1 && fromBoolean value2

evaluateBooleanOr :: Object -> Object -> StateMonad Object
evaluateBooleanOr (BuiltinObject (BoolObject value1)) (BuiltinObject (BoolObject value2)) =
    return $ BuiltinObject $ BoolObject $ toBoolean $ fromBoolean value1 || fromBoolean value2

evaluateEquality :: Object -> Object -> StateMonad Object
evaluateEquality value1 value2 = return $ BuiltinObject $ BoolObject $ toBoolean $ value1 == value2

evaluateNonEquality :: Object -> Object -> StateMonad Object
evaluateNonEquality value1 value2 = return $ BuiltinObject $ BoolObject $ toBoolean $ value1 /= value2
