module ExPg455 where

import Data.Char
import Data.List

isSubseqOf :: (Eq a)  => [a] -> [a] -> Bool
isSubseqOf xs ys = let res = foldl' linearSearch (True, ys) xs
                   in fst res

linearSearch :: (Eq a) => (Bool, [a]) -> a -> (Bool, [a])
linearSearch (_, []) _ = (False, [])
linearSearch (b, w:ws) c
             | c == w = (True && b, ws)
             | otherwise = linearSearch (b, ws) c


capitalizeWord :: String -> String
capitalizeWord (w:ws) = (toUpper w) : ws
capitalizeWord [] = []

capitalizeWords :: String -> [(String, String)]
capitalizeWords s = map capTuple $ words s
    where capTuple [] = ("", "")
          capTuple w@(x:xs) = (w, (toUpper x) : xs)

capitalizeParagraph :: String -> String
capitalizeParagraph pp = let caps = fmap (capitalizeWord . dropSpaces) $ splitAndStrip '.' pp
                         in (intercalate ".  " caps) ++ "."
                         where dropSpaces = dropWhile (\c -> c == ' ')

splitAndStrip :: (Eq a) => a -> [a] -> [[a]]
splitAndStrip = mySplit []

-- Recurses over span output
mySplit :: (Eq a) => [[a]] -> a -> [a] -> [[a]]
mySplit accum _ [] = accum
mySplit accum x xs = let (s, rest) = span (\c -> c /= x) xs
                         rest'     = dropWhile (\c -> c == x) rest
                     in mySplit (accum ++ [s]) x rest'
