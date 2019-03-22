module Main where

import qualified Data.Map as M
import Morse
import Test.QuickCheck

allowedChars :: [Char]
allowedChars = M.keys letterToMorse

allowedMorse :: [Morse]
allowedMorse = M.elems letterToMorse

genChars :: Gen Char
genChars = elements allowedChars

genMorse :: Gen Morse
genMorse = elements allowedMorse

prop_thereAndBack :: Property
prop_thereAndBack =
    forAll genChars
           (\c -> ((charToMorse c) >>= morseToChar) == Just c)

main :: IO ()
main = quickCheck prop_thereAndBack
