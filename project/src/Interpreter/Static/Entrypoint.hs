module Interpreter.Static.Entrypoint (interpret) where

import           Control.Exception
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Maybe
import           System.Exit

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
    executeStaticCheck mode classEnv classDeclarations
    executeRuntime mode classEnv (fromJust mainClass)


executeStaticCheck :: Mode -> ClassEnv -> [ClassDecl] -> IO ()
executeStaticCheck mode classEnv classDeclarations = do
    staticCheckResult <- case mode of
        Debug -> performStaticCheck mode classEnv classDeclarations
        Production -> catchAny (performStaticCheck mode classEnv classDeclarations) handleUnexpectedError
    case staticCheckResult of Left error -> exitWithError $ show error
                              Right _    -> return ()

executeRuntime :: Mode -> ClassEnv -> ClassDecl -> IO ()
executeRuntime mode classEnv mainClass = do
    runtimeResult <- case mode of
        Debug -> runtime mode classEnv mainClass
        Production -> catchAny (runtime mode classEnv mainClass) handleUnexpectedException
    case runtimeResult of Left error        -> case error of UserExitException code -> handleUserExit code
                                                             _ -> exitWithError $ show error
                          Right returnValue -> print returnValue
    -- TODO when System#print etc. is there: debugPrint mode "Return value" returnValue, instead of this print

performStaticCheck :: Mode -> ClassEnv -> [ClassDecl] -> IO (Either CompilationError ObjectType)
performStaticCheck mode classEnv classDeclarations = runExceptT
    $ runReaderT (checkClasses classDeclarations) (initialStaticEnvironment classEnv)

catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = Control.Exception.catch

handleUnexpectedException :: SomeException -> IO (Either RuntimeException Object)
handleUnexpectedException _ = do
    let exception = RuntimeException "Unexpected exception"
    exitWithError $ show exception
    return $ Left exception

handleUnexpectedError :: SomeException -> IO (Either CompilationError ObjectType)
handleUnexpectedError _ = do
    let error = CompilationError "Unexpected error"
    exitWithError $ show error
    return $ Left error

handleUserExit :: Int -> IO ()
handleUserExit code = do
    if code == 0 then exitSuccess
    else exitWith $ ExitFailure code
