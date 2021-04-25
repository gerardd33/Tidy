module Main where

import           System.Directory   (doesFileExist, setCurrentDirectory)
import           System.Environment (getArgs)
import           System.Exit        (exitFailure)
import           System.IO

import           Interpreter.Main
import           Parser.Tidy.Abs
import           Parser.Tidy.Lex
import           Parser.Tidy.Par


main :: IO ()
main = do
    -- Changing the default directory to Tidy project root instead of the stack project root.
    setCurrentDirectory ".."
    args <- getArgs
    case args of
        []           -> usage
        [sourceFile] -> interpretFile sourceFile
        _            -> usage


lexer :: String -> [Token]
lexer = myLexer

parser :: [Token] -> Either String Program
parser = pProgram

interpretFile :: String -> IO ()
interpretFile filePath = do
    fileExists <- doesFileExist filePath
    if not fileExists then do
        hPutStrLn stderr $ "Error: file " ++ filePath ++ " does not exist."
    else do
        source <- readFile filePath
        case (parser . lexer) source of
            Left error -> do
                hPutStrLn stderr $ "Error: " ++ error
                exitFailure
            Right astTree -> interpret astTree


usage :: IO ()
usage = putStrLn "Usage: ./tidy source_file_path"
