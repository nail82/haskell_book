module Main where

import Data.List
import ComboLib

main :: IO ()
main =
  putStrLn ws
  where ws = intercalate "\n" $ fmap show threeLetterCombos
