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


interpret :: Mode -> Program -> IO ()
interpret mode (ProgramEntrypoint classDeclarations) = do
    debugPrint mode "Loaded classes" classDeclarations
    let classEnv = loadClasses classDeclarations
    let mainClass = findMainClass classDeclarations
    when (isNothing mainClass) $ exitWithError $ show NoMainActionError
    debugPrint mode "Main action" $ getMainAction $ fromJust mainClass
    staticCheckResult <- performStaticCheck mode classEnv classDeclarations
    case staticCheckResult of Left error -> exitWithError $ show error
                              Right _    -> return ()
    runtimeResult <- runtime mode classEnv $ fromJust mainClass
    case runtimeResult of Left error -> exitWithError $ show error
                          Right returnValue -> debugPrint mode "Return value" returnValue


performStaticCheck :: Mode -> ClassEnv -> [ClassDecl] -> IO (Either CompilationError ObjectType)
performStaticCheck mode classEnv classDeclarations = runExceptT
    $ runReaderT (checkClasses classDeclarations) (initialStaticEnvironment classEnv)
