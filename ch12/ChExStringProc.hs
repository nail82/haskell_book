module ChExStringProc where

import Data.Char

notThe :: String -> Maybe String
notThe w
       | w == "the" = Nothing
       | otherwise  = Just w


-- Clearly some symmetries here
checkWord :: (Maybe String, String) -> String
checkWord (Nothing, _)  = "a"
checkWord (Just w, _) = w

checkVowel :: (Maybe String, String) -> Integer
checkVowel (Nothing, (w:_)) = charToInt w
checkVowel _ = 0

-- More symmetry here
replaceThe :: String -> String
replaceThe "" = ""
replaceThe s  = checkWord (notThe first, rest) ++ " " ++ replaceThe rest
    where (first, rest) = trimmedSpan s

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel "" = 0
countTheBeforeVowel s  = checkVowel (notThe first, rest) + countTheBeforeVowel rest
    where (first, rest) = trimmedSpan s


-- Custom span that removes the leading whitespace from rest
trimmedSpan :: String -> (String, String)
trimmedSpan s  = (first, rest')
    where (first, rest) = span ((/=) ' ') s
          rest'  = dropWhile ((==) ' ') rest


countVowels :: String -> Integer
countVowels s = sum $ map charToInt s

charToInt :: Char -> Integer
charToInt c
    | c' `elem` "aeiou" = 1
    | otherwise         = 0
    where c' = toLower c
