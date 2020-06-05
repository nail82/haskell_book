module Ex7 where

import Data.Bits
import Data.Word
import Text.Trifecta

data IPAddress6 =
    IPAddress6 Word64 Word64
    deriving (Eq, Ord, Show)
