module Ex6 where

-- Parse IPV4 addresses

import Data.Word
import Data.Bits
import Text.Trifecta

data IPAddress =
    IPAddress Word32
    deriving (Eq, Ord, Show)

parseQuad :: Parser IPAddress
parseQuad = do
  cs <- sepBy1 parseAddressComponent dot
  if length cs == 4 then return $ IPAddress $ shiftQuad cs 0
  else fail "Too many components"

parseAddressComponent :: Parser Word32
parseAddressComponent = do
  c <- integer
  if 0 <= c && c < 256 then return $ fromInteger c
  else fail "Component out of range"

shiftQuad :: [Word32] -> Word32 -> Word32
shiftQuad [] v = v
shiftQuad (c:cs) v =
    let s = length cs * 8
        v' = (shift c s) .|. v
    in shiftQuad cs v'
