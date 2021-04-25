module Interpreter.Main (interpret) where

import           Commons
import qualified Data.Map        as Map
import           Parser.Tidy.Abs

type ClassEnv = Map.Map String ClassDecl

-- TODO static type checking before evaluation
interpret :: Mode -> Program -> IO ()
interpret mode (ProgramEntrypoint classDeclarations) = do
    ifDebug mode $ mapM_ print classDeclarations >> putStrLn ""
    print classEnv
    where classEnv = loadClasses classDeclarations emptyClassEnv


emptyClassEnv :: ClassEnv
emptyClassEnv = Map.empty

loadClasses :: [ClassDecl] -> ClassEnv -> ClassEnv
loadClasses declarations classEnv = Map.fromList $ map evalClassDeclaration declarations

-- TODO static verification of many things in evaluation functions, e.g. if class has only allowed sections
evalClassDeclaration :: ClassDecl -> (String, ClassDecl)
evalClassDeclaration decl = case decl of
    (ClassDeclConcrete modifier (CIdent (UpperCaseIdent name)) inheritance body) -> (name, decl)
    (ClassDeclAbstract modifier (CIdent (UpperCaseIdent name)) inheritance body) -> (name, decl)
