module Interpreter.Common.Helper.Types where

import           Interpreter.Common.Types
import           Parser.Tidy.Abs


toBoolean :: Bool -> Boolean
toBoolean True  = BTrue
toBoolean False = BFalse

fromBoolean :: Boolean -> Bool
fromBoolean BTrue  = True
fromBoolean BFalse = False

isTrue :: Object -> Bool
isTrue (BuiltinObject (BoolObject BTrue))  = True
isTrue (BuiltinObject (BoolObject BFalse)) = False
-- TODO exception in other cases
