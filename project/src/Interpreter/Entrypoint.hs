module Interpreter.Entrypoint (interpret) where

import qualified Data.Map                as Map
import           Data.Maybe

import           Interpreter.Classes
import           Interpreter.Commons
import           Interpreter.Environment
import           Interpreter.Runtime     (runtime)
import           Parser.Tidy.Abs


-- TODO static type checking before evaluation
-- TODO better separation of class loading and main execution, for now a POC
interpret :: Mode -> Program -> IO ()
interpret mode (ProgramEntrypoint classDeclarations) = do
    debugPrint mode "Loaded classes" classDeclarations
    let classEnv = loadClasses classDeclarations emptyClassEnv
    let mainClass = findMainClass classDeclarations
    debugPrint mode "Main action" $ fromJust $ getMainAction mainClass
    result <- runtime mode classEnv mainClass
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

-- TODO temporary behaviour, expand it later
-- TODO here print NoMainActionError and terminate if list empty,
--  once I have monads adapted for static checking
findMainClass :: [ClassDecl] -> ClassDecl
findMainClass = head . filter hasMainAction


-- TODO further things to check statically:
-- make sure expression lists in action bodies are not empty
-- not allowing uninitialized values outside of a class and checking them inside a class
