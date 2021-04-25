module Main where

import           System.Directory   (doesFileExist, setCurrentDirectory)
import           System.Environment (getArgs)
import           System.Exit        (exitFailure)
import           System.IO

import           Parser.Tidy.Abs
import           Parser.Tidy.Lex
import           Parser.Tidy.Par


lexer :: String -> [Token]
lexer = myLexer

parser :: [Token] -> Either String Program
parser = pProgram


interpret :: Program -> IO ()
interpret astTree = putStrLn "TODO"


interpretFile :: String -> IO ()
interpretFile filePath = do
    fileExists <- doesFileExist filePath
    if fileExists then do
        source <- readFile filePath
        case (parser . lexer) source of
            Left error -> do
                hPutStrLn stderr $ "Error: parsing failed: " ++ error
                exitFailure
            Right astTree -> interpret astTree
    else do
        hPutStrLn stderr $ "Error: file " ++ filePath ++ " does not exist."

usage :: IO ()
usage = putStrLn "Usage: ./tidy source_file_path"

main :: IO ()
main = do
    -- Changing the default directory to Tidy project root instead of the stack project root
    setCurrentDirectory ".."
    args <- getArgs
    case args of
        []           -> usage
        [sourceFile] -> interpretFile sourceFile
        _            -> usage
