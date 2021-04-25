module Commons where

import           Control.Monad

data Mode = Debug | Production
    deriving (Eq, Show)

ifDebug :: Applicative f => Mode -> f () -> f ()
ifDebug mode = when (mode == Debug)
