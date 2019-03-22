module HangmanLib
    (
     gameOver
    , gameWin
    , handleGuess
    , randomWord'
    , freshPuzzle
    , Puzzle
    , fillInCharacter
    , fillSlots
    , allWords
    )
    where

import System.Exit (exitSuccess)
import Data.List (intersperse)
import System.Random (randomRIO)
import Data.Maybe (isJust)

newtype WordList =
    WordList [String]
    deriving (Eq, Show)

data Puzzle =
  Puzzle String [Maybe Char] [Char]

instance Show Puzzle where
    show (Puzzle _ discovered guessed) =
        (intersperse ' ' $
         fmap renderPuzzleChar discovered)
        ++ " Guessed so far: " ++ guessed

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return $ WordList (lines dict)

gameWords :: IO WordList
gameWords = do
  WordList aw <- allWords
  return $ WordList (filter myFilt aw)
  where myFilt w =
            let l = length w
            in minWordLength <= l && l < maxWordLength

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  idx <- randomRIO (0, length wl)
  return $ wl !! idx

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just c) = c

freshPuzzle :: String -> Puzzle
freshPuzzle w = Puzzle w hidden ""
                where hidden = take (length w) $ repeat Nothing

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle puzWord _ _) guess = elem guess puzWord

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guesses) g = elem g guesses

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle puzWord hidden guesses) guess =
    Puzzle puzWord hidden'  (guess : guesses)
    where hidden' = zipWith myZipper hidden (fillSlots puzWord guess)

myZipper :: Maybe Char -> Maybe Char -> Maybe Char
myZipper (Just a) _       = Just a
myZipper Nothing (Just a) = Just a
myZipper Nothing Nothing  = Nothing

fillSlots :: String -> Char -> [Maybe Char]
fillSlots puzWord guess = fmap (matchingChar guess) puzWord

matchingChar :: Char -> Char -> Maybe Char
matchingChar guess c
    | guess == c = Just c
    | otherwise = Nothing


fillInCharacter' :: Puzzle -> Char -> Puzzle
fillInCharacter' (Puzzle word filledInSoFar guesses) c =
    Puzzle word newFilledInSoFar (c : guesses)
    where newFilledInSoFar =
              zipWith (bookZipper c)
                      word filledInSoFar

-- From the book chapter
bookZipper :: Char -> Char -> Maybe Char -> Maybe Char
bookZipper guessed wordChar guessChar =
    if wordChar == guessed
    then Just wordChar
    else guessChar


handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess pz guess = do
  putStr $ "You guessed => " ++ [guess] ++ ".  "
  case (charInWord pz guess,
       alreadyGuessed pz guess) of
    (_, True) -> do
      putStrLn "You already guessed that..."
      return pz
    (True, _) -> do
      putStrLn "Good guess..."
      return (fillInCharacter pz guess)
    (False, _) -> do
      putStrLn "Unlucky..."
      return (fillInCharacter pz guess)

gameOver :: Puzzle -> IO ()
gameOver (Puzzle puzWord _ guesses) =
           if (length guesses) > 7 then
               do putStrLn "You lost!"
                  putStrLn $ "The word was => " ++ puzWord
                  exitSuccess
           else return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ hidden _) =
    if all isJust hidden then
        do putStrLn "You Win!"
           exitSuccess
    else return ()
