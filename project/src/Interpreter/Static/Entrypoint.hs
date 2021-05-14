module Interpreter.Static.Entrypoint (interpret) where

import           Data.Maybe

import           Interpreter.Common.Types
import           Parser.Tidy.Abs

import           Interpreter.Common.Debug
import           Interpreter.Common.Errors
import           Interpreter.Common.Utils.Classes
import           Interpreter.Runtime.Entrypoint


-- TODO static type checking before evaluation
interpret :: Mode -> Program -> IO ()
interpret mode (ProgramEntrypoint classDeclarations) = do
    debugPrint mode "Loaded classes" classDeclarations
    let classEnv = loadClasses classDeclarations
    let mainClass = findMainClass classDeclarations
    debugPrint mode "Main action" $ fromJust $ getMainAction mainClass
    result <- runtime mode classEnv mainClass
    case result of Left error -> exitWithError $ show error
                   Right returnValue -> debugPrint mode "Return value" returnValue


-- TODO further things to check statically:
-- make sure expression lists in action bodies are not empty
-- not allowing uninitialized values outside of a class and checking them inside a class
