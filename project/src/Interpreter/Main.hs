module Interpreter.Main (interpret) where

import qualified Data.Map        as Map
import           Parser.Tidy.Abs

type ClassEnv = Map.Map ClassIdent ClassDecl

-- TODO static type checking before evaluation
interpret :: Program -> IO ()
interpret (ProgramEntrypoint classDeclarations) = do
    print classEnv
    where classEnv = loadClasses classDeclarations emptyClassEnv


emptyClassEnv :: ClassEnv
emptyClassEnv = Map.empty

loadClasses :: [ClassDecl] -> ClassEnv -> ClassEnv
loadClasses declarations classEnv = classEnv
