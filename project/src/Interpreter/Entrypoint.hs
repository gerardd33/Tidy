module Interpreter.Entrypoint (interpret) where

import           Commons
import qualified Data.Map                as Map
import           Interpreter.Environment
import           Interpreter.Runtime     (runtime)
import           Parser.Tidy.Abs

-- TODO static type checking before evaluation
-- TODO better separation of class loading and main execution, for now a POC
interpret :: Mode -> Program -> IO ()
interpret mode (ProgramEntrypoint classDeclarations) = do
    ifDebug mode $ mapM_ print classDeclarations >> putStrLn ""
    let classEnv = loadClasses classDeclarations emptyClassEnv
    ifDebug mode $ print classEnv
    result <- runtime mode classEnv
    print result


emptyClassEnv :: ClassEnv
emptyClassEnv = Map.empty

loadClasses :: [ClassDecl] -> ClassEnv -> ClassEnv
loadClasses declarations classEnv = Map.fromList $ map evalClassDeclaration declarations

-- TODO static verification of many things in evaluation functions, e.g. if class has only allowed sections
evalClassDeclaration :: ClassDecl -> (String, ClassDecl)
evalClassDeclaration decl = case decl of
    (ClassDeclConcrete modifier (CIdent (UpperCaseIdent name)) inheritance body) -> (name, decl)
    (ClassDeclAbstract modifier (CIdent (UpperCaseIdent name)) inheritance body) -> (name, decl)
