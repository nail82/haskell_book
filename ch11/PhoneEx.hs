module PhoneEx where

import Data.List
import Data.Char

-- We've looked at sums, products and sums of products so far
data DaPhone = DaPhone [(Char, String)] deriving (Show)

myPhone :: DaPhone
myPhone = DaPhone [('1', "1")
                  ,('2', "abc2")
                  ,('3', "def3")
                  ,('4', "ghi4")
                  ,('5', "jkl5")
                  ,('6', "mno6")
                  ,('7', "pqrs7")
                  ,('8', "tuv8")
                  ,('9', "wxyz9")
                  ,('*', "^*")
                  ,('0', "+ 0")
                  ,('#', ".,#")]


convo :: [String]
convo =
    ["Wanna play 20 questions"
    , "Ya"
    , "U 1st haha"
    , "Lol ok.  Have u ever tasted alcolhol"
    , "Lol ya"
    , "Wow ur cool haha. Ur turn"
    , "Ok. Do u think I am pretty Lol"
    , "Lol ya"
    , "Just making sure rofl ur turn"]

lcConvo :: [String]
lcConvo =
    ["wanna play 20 questions"
    , "ya"
    , "u 1st haha"
    , "lol ok.  have u ever tasted alcolhol"]

myShift :: (Digit, Presses)
myShift = ('*', 1)

unPrintable :: (Digit, Presses)
unPrintable = ('*', 2)

type Digit = Char
type Presses = Int

-- I'm using pressLookup twice here and shouldn't.  Rethink.
-- Do this for a single button (vice the whole phone),
-- with invalid handling, then map a string to this function.
buttonLookup :: DaPhone -> Char -> (Digit, Presses)
buttonLookup (DaPhone bs) c = let xs = filter (buttonFilter c) bs
                              in if length xs == 0
                                 then unPrintable
                                 else pressLookup (head xs) c

buttonFilter :: Char -> (Char, String) -> Bool
buttonFilter c t = (snd $ pressLookup t c) /= 0

pressLookup :: (Char, String) -> Char -> (Digit, Presses)
pressLookup (b, s) c = let i = elemIndex (toLower c) s
                       in (b, (resolveIdx i) + 1)

resolveIdx :: Maybe Int -> Int
resolveIdx Nothing  = -1
resolveIdx (Just i) = i


reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps phone c = if isUpper c
                      then myShift : [buttonLookup phone c]
                      else [buttonLookup phone c]

shiftedLetters :: DaPhone -> String -> [[(Digit, Presses)]]
shiftedLetters phone con = fmap (reverseTaps phone) con

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead phone con = concat $ shiftedLetters phone con

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = foldr (\t x -> (snd t) + x) 0

mostPopularLetter :: String -> Maybe Char
mostPopularLetter [] = Nothing
mostPopularLetter ss = Just (head
                             $ last
                                   $ sortOn length
                                         $ group
                                               $ sort
                                                     $ fmap toLower ss)


coolestLtr :: [String] -> Maybe Char
coolestLtr ss = mostPopularLetter $ concat ss

coolestWord :: [String] -> String
coolestWord [] = ""
coolestWord con = let ws = concat $ fmap (words . toLowerString) con
                  in head $ last $ sortOn length $ group $ sort ws

toLowerString :: String -> String
toLowerString s = fmap toLower s



-- This is extra.  Wanted to make sure I was properly encoding values
-- Using kleene star as a replacement for something unprintable
decodeLetter :: DaPhone -> [(Digit, Presses)] -> Char
decodeLetter _ [] = '*'
decodeLetter phone (t:[]) = lookupLetter phone t
decodeLetter phone (t:u:_) = if t == myShift
                             then toUpper $ lookupLetter phone u
                             else lookupLetter phone t

lookupLetter :: DaPhone -> (Digit, Presses) -> Char
lookupLetter (DaPhone bs) t = let letters = lookup (fst t) bs
                              in indexLetters letters (snd t)
    where indexLetters (Just s) i = last $ take i $ cycle s
          indexLetters Nothing _ = '*'
