module Interpreter.Entrypoint (interpret) where

import           Data.Maybe

import           Interpreter.Classes
import           Interpreter.Commons
import           Interpreter.Runtime (runtime)
import           Interpreter.State
import           Parser.Tidy.Abs


-- TODO static type checking before evaluation
-- TODO better separation of class loading and main execution, for now a POC
interpret :: Mode -> Program -> IO ()
interpret mode (ProgramEntrypoint classDeclarations) = do
    debugPrint mode "Loaded classes" classDeclarations
    let classEnv = loadClasses classDeclarations
    let mainClass = findMainClass classDeclarations
    debugPrint mode "Main action" $ fromJust $ getMainAction mainClass
    result <- runtime mode classEnv mainClass
    print result


-- TODO further things to check statically:
-- make sure expression lists in action bodies are not empty
-- not allowing uninitialized values outside of a class and checking them inside a class
