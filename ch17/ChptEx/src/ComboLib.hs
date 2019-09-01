module ComboLib where

import Control.Applicative (liftA3)

-- Pg 742

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos as bs cs = liftA3 (,,) as bs cs

threeLetterCombos :: [(Char, Char, Char)]
threeLetterCombos = combos stops vowels stops
