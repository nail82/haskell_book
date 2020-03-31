module Main where

import LearnParsers
import Text.Fractions
import Text.Trifecta

main :: IO ()
main = do
--  pNL "stop:"
--  testParse stop
  pNL "one:"
  testParse one
  pNL "oneTwo:"
  testParse oneTwo
  pNL "oneTwo':"
  testParse oneTwo'
  pNL "oneTwoEof:"
  testParse oneTwoEof

  let virtuousFraction' =
          parseString virtuousFraction mempty
  print $ virtuousFraction' shouldWork
  print $ virtuousFraction' shouldAlsoWork
  print $ virtuousFraction' alsoBad
  print $ virtuousFraction' badFraction
