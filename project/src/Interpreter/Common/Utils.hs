module Interpreter.Common.Utils where

import           Control.Monad
import           Parser.Tidy.Abs


data Mode = Debug | Production
    deriving (Eq, Show)

ifDebug :: Applicative f => Mode -> f () -> f ()
ifDebug mode = when (mode == Debug)

debugPrint :: (Show a) => Mode -> String -> a -> IO ()
debugPrint mode header item = ifDebug mode $ putStrLn ("DEBUG: " ++ header) >> print item >> putStrLn ""

debugLog :: Mode -> String -> IO ()
debugLog mode message = ifDebug mode $ putStrLn ("DEBUG: " ++ message) >> putStrLn ""


classIdent :: String -> ClassIdent
classIdent identifier = CIdent (UpperCaseIdent identifier)
