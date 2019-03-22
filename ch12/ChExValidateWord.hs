module ChExValidateWord where

import Data.Char

newtype Word' =
    Word' String
    deriving (Eq, Show)

vowels :: String
vowels = "aeiou"

consonants :: String
consonants = "bcdfghjklmnpqrstvwxyz"

mkWord :: String -> Maybe Word'
mkWord s
    | vc > cc = Nothing
    | otherwise = Just (Word' s)
    where (LetterCount vc cc) = foldr countLetter (LetterCount 0 0) s


type VowelCount     = Integer
type ConsonantCount = Integer

data LetterCount = LetterCount VowelCount ConsonantCount
    deriving (Eq, Show)

countLetter :: Char -> LetterCount -> LetterCount
countLetter c lc@(LetterCount vc cc)
            | toLower c `elem` vowels     = LetterCount (vc + 1) cc
            | toLower c `elem` consonants = LetterCount vc (cc + 1)
            | otherwise                   = lc
