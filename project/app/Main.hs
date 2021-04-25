module Main where

import           System.Environment
import           System.Exit
import           System.IO

import           Parser.Tidy.Abs
import           Parser.Tidy.Lex
import           Parser.Tidy.Par

usage :: IO ()
usage = putStrLn "Usage: ./tidy source_file_path"

main :: IO ()
main = do
    args <- getArgs
    case args of
        []           -> usage
        [sourceFile] -> putStrLn "TODO"
        _            -> usage
