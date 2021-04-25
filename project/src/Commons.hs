module Commons where

import           Control.Monad
import qualified Data.Map        as Map
import           Parser.Tidy.Abs


type ClassEnv = Map.Map String ClassDecl


data Mode = Debug | Production
    deriving (Eq, Show)

ifDebug :: Applicative f => Mode -> f () -> f ()
ifDebug mode = when (mode == Debug)
