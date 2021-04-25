module Interpreter.Main (interpret) where

import           Commons
import qualified Data.Map        as Map
import           Parser.Tidy.Abs

type ClassEnv = Map.Map ClassIdent ClassDecl

-- TODO static type checking before evaluation
interpret :: Mode -> Program -> IO ()
interpret mode (ProgramEntrypoint classDeclarations) = do
    -- print classDeclarations -- TODO remove debug
    print classEnv
    where classEnv = loadClasses classDeclarations emptyClassEnv


emptyClassEnv :: ClassEnv
emptyClassEnv = Map.empty

loadClasses :: [ClassDecl] -> ClassEnv -> ClassEnv
loadClasses declarations classEnv = classEnv
