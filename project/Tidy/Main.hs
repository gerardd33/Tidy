module Main where

import System.Environment ( getArgs, getProgName )
import System.Exit        ( exitFailure, exitSuccess )

import Tidy.Lex   ( Token )
import Tidy.Par   ( pProgram, myLexer )
import Tidy.Skel  ()
import Tidy.Print ( Print, printTree )
import Tidy.Abs   ()

type Err = Either String
type ParseFunction a = [Token] -> Err a

lexer = myLexer


usage :: IO ()
usage = putStrLn "Usage: ./tidy source_file_path"

showTree :: (Show a, Print a) => a -> IO ()
showTree tree = do
      putStrLn $ "\n[Abstract Syntax]\n\n" ++ show tree
      putStrLn $ "\n[Linearized tree]\n\n" ++ printTree tree

runProgram :: (Print a, Show a) => ParseFunction a -> String -> IO ()
runProgram parser sourceCode = case parser lexedSource of
    Left sourceCode -> do
      putStrLn "Syntax error:\n"
      print lexedSource
      putStrLn sourceCode
      exitFailure
    Right astTree -> do
      putStrLn "\nParse Successful!"
      showTree astTree
      exitSuccess
  where lexedSource = lexer sourceCode

runFile :: (Print a, Show a) => ParseFunction a -> FilePath -> IO ()
runFile parser filePath = putStrLn filePath >> readFile filePath >>= runProgram parser


main :: IO ()
main = do
    args <- getArgs
    case args of
        ["--help"] -> usage
        filePath -> mapM_ (runFile pProgram) filePath
        _ -> usage
