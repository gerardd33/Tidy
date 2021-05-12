module Interpreter.Eval.Environment where

import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.Map                 as Map
import           Data.Maybe

import           Interpreter.Common.Types
import           Parser.Tidy.Abs


getLocation :: ObjectIdent -> StateMonad Location
getLocation objectName = do
    (localEnv, _) <- ask
    return $ fromJust $ Map.lookup objectName localEnv

getLocalValue :: ObjectIdent -> StateMonad Object
getLocalValue objectName = do
    location <- getLocation objectName
    (state, _) <- get
    return $ fromJust $ Map.lookup location state
