module Interpreter.Entrypoint.Static (interpret) where

import           Data.Maybe

import           Interpreter.Common.Types
import           Parser.Tidy.Abs

import           Interpreter.Common.Debug
import           Interpreter.Common.Helper.Classes
import           Interpreter.Entrypoint.Runtime



-- TODO static type checking before evaluation
interpret :: Mode -> Program -> IO ()
interpret mode (ProgramEntrypoint classDeclarations) = do
    debugPrint mode "Loaded classes" classDeclarations
    let classEnv = loadClasses classDeclarations
    let mainClass = findMainClass classDeclarations
    debugPrint mode "Main action" $ fromJust $ getMainAction mainClass
    result <- runtime mode classEnv mainClass
    debugPrint mode "Result" result


-- TODO further things to check statically:
-- make sure expression lists in action bodies are not empty
-- not allowing uninitialized values outside of a class and checking them inside a class
