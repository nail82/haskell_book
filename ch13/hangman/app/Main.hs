module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import HangmanLib

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
                   gameOver puzzle
                   gameWin  puzzle
                   putStrLn $ "Current puzzle is: " ++ show puzzle
                   putStr "Guess a letter => "
                   guess <- getLine
                   case guess of
                     [c] -> handleGuess puzzle c >>= runGame
                     _   -> putStrLn "Guess with a single character"

main :: IO ()
main = do
  word <- randomWord'
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle
