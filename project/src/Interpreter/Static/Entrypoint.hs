module Interpreter.Static.Entrypoint (interpret) where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Maybe

import           Interpreter.Common.Types
import           Parser.Tidy.Abs

import           Interpreter.Common.Debug
import           Interpreter.Common.Errors
import           Interpreter.Common.Utils.Classes
import           Interpreter.Common.Utils.Environments
import           Interpreter.Runtime.Entrypoint
import           Interpreter.Static.Classes


-- TODO static type checking before evaluation
interpret :: Mode -> Program -> IO ()
interpret mode (ProgramEntrypoint classDeclarations) = do
    debugPrint mode "Loaded classes" classDeclarations
    let classEnv = loadClasses classDeclarations
    let mainClass = findMainClass classDeclarations
    when (isNothing mainClass) $ exitWithError $ show NoMainActionError
    debugPrint mode "Main action" $ getMainAction $ fromJust mainClass
    staticCheckResult <- checkStatically mode classEnv classDeclarations
    case staticCheckResult of Left error -> exitWithError $ show error
                              Right _ -> return ()
    result <- runtime mode classEnv $ fromJust mainClass
    case result of Left error -> exitWithError $ show error
                   Right returnValue -> debugPrint mode "Return value" returnValue


checkStatically :: Mode -> ClassEnv -> [ClassDecl] -> IO (Either CompilationError StaticCheckEnv)
checkStatically mode classEnv classDeclarations = runExceptT
    $ runReaderT (checkClasses classDeclarations) (initialStaticCheckEnvironment classEnv)
