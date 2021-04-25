module Lib
    ( someFunc
    ) where

import Parser.Tidy.Abs

someFunc :: IO ()
someFunc = putStrLn $ showClassName $ CIdent (UpperCaseIdent "Student")

showClassName :: ClassIdent -> String
showClassName (CIdent (UpperCaseIdent name)) = name
