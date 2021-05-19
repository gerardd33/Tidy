module Main where

import           Parser.Tidy.Abs
import           Parser.Tidy.Lex               (Token)
import           Parser.Tidy.Par               (myLexer, pProgram)

import           Interpreter.Common.Debug
import           Interpreter.Common.Errors
import           Interpreter.Static.Entrypoint (interpret)

import           System.Directory              (doesFileExist,
                                                setCurrentDirectory)
import           System.Environment            (getArgs)
import           System.IO


main :: IO ()
main = do
    -- Changing the default directory to Tidy project root instead of the stack project root.
    setCurrentDirectory ".."
    args <- getArgs
    case args of
        []                     -> usage
        "--debug":[sourceFile] -> interpretFile Debug sourceFile
        sourceFile:["--debug"] -> interpretFile Debug sourceFile
        [sourceFile]           -> interpretFile Production sourceFile
        _                      -> usage


lexer :: String -> [Token]
lexer = myLexer

parser :: [Token] -> Either String Program
parser = pProgram

usage :: IO ()
usage = putStrLn "Usage: ./tidy source_file_path [--debug]"

interpretFile :: Mode -> String -> IO ()
interpretFile mode filePath = do
    debugLog mode "Tidy interpreter running in DEBUG mode."
    fileExists <- doesFileExist filePath
    if not fileExists then exitWithError $ "Source file " ++ filePath ++ " does not exist."
    else interpretFileContents mode filePath

interpretFileContents :: Mode -> String -> IO ()
interpretFileContents mode filePath = do
    source <- readFile filePath
    case (parser . lexer) source of
        Left error    -> exitWithError error
        Right astTree -> interpret mode astTree
