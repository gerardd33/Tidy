module Interpreter.Eval.Utils where

import           Control.Monad.Reader
import qualified Data.Map                          as Map

import           Interpreter.Common.Types
import           Parser.Tidy.Abs

import           Interpreter.Common.Helper.Objects


returnPure :: StateMonad Result -> StateMonad Object
returnPure calculation = do
    (value, _) <- calculation
    return value

liftPure :: StateMonad Object -> StateMonad Result
liftPure calculation = do
    env <- ask
    value <- calculation
    return (value, env)

returnPass :: StateMonad Result
returnPass = liftPure $ return pass

getClassDecl :: ClassIdent -> StateMonad ClassDecl
getClassDecl classIdent = do
    (_, classEnv) <- ask
    return $ classEnv Map.! classIdent
