module WordNumber where

import Data.List (intersperse)

wordNumber :: Int -> String
wordNumber n = toString . mapDigits . digits $ n

digits :: Int -> [Int]
digits n = reverse $ go n []
    where
      go n' xs
          | n' < 10 = n' : xs
          | otherwise = t2 : go t1 xs
          where (t1, t2) = divMod n' 10

mapDigits :: [Int] -> [String]
mapDigits [] = []
mapDigits (x:xs) = digitToWord x : mapDigits xs

toString :: [String] -> String
toString xs = concat $ intersperse "-" xs

digitToWord :: Int -> String
digitToWord n =
    case n of
      0 -> "zero"
      1 -> "one"
      2 -> "two"
      3 -> "three"
      4 -> "four"
      5 -> "five"
      6 -> "six"
      7 -> "seven"
      8 -> "eight"
      9 -> "nine"
      _ -> "oops"


digitsFixed :: Int -> [Int]
digitsFixed n = go (abs n)
    where
      go 0 = []
      go n = go ( n `div` 10) ++ [n `mod` 10]
