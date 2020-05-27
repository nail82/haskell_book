module Ex6 where

-- Parse IPV4 addresses

import Data.Word
import Data.Bits
import Text.Trifecta

data IPAddress =
    IPAddress Word32
    deriving (Eq, Ord, Show)
