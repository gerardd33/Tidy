module Commons where

import           Control.Monad
import           Parser.Tidy.Abs


data Mode = Debug | Production
    deriving (Eq, Show)

ifDebug :: Applicative f => Mode -> f () -> f ()
ifDebug mode = when (mode == Debug)
