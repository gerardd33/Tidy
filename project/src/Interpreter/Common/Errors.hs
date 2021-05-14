module Interpreter.Common.Errors where

import           System.Exit (exitFailure)
import           System.IO


-- TODO other exceptions
-- TODO better printing (custom show instance)
data RuntimeException
    = DivideByZeroException
    | RuntimeException String
    deriving (Show)

data CompilationError
    = NoMainMethodError
    | CompilationError String
    deriving (Show)


exitWithError :: String -> IO ()
exitWithError error = hPutStrLn stderr ("Error: " ++ error) >> exitFailure
