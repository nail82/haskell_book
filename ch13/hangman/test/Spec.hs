module Main where

import Test.QuickCheck
import Test.QuickCheck.All
import HangmanLib

prop_Addition :: Int -> Bool
prop_Addition x = x + 1 > x

-- TODO: Finish these exercises


main :: IO ()
main = quickCheck prop_Addition
