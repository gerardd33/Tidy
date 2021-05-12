module Interpreter.Eval.Expressions.Miscellaneous where

import           Interpreter.Common.Types
import           Parser.Tidy.Abs

import           Interpreter.Common.Helper.Objects


evalLiteral :: Literal -> StateMonad Object
evalLiteral (LInt int)       = return (newBuiltinObject (IntObject int))
evalLiteral (LBool bool)     = return (newBuiltinObject (BoolObject bool))
evalLiteral (LChar char)     = return (newBuiltinObject (CharObject char))
evalLiteral (LString string) = return (newBuiltinObject (StringObject string))
evalLiteral (LVoid void)     = return (newBuiltinObject VoidObject)









