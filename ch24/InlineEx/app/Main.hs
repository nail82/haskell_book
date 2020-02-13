module Main where

import LearnParsers

main :: IO ()
main = do
  pNL "stop:"
  testParse stop
  pNL "one:"
  testParse one
  pNL "oneTwo:"
  testParse oneTwo
  pNL "oneTwo':"
  testParse oneTwo'
  pNL "oneEof:"
  testParse oneEof
  pNL "oneTwoEof:"
  testParse oneTwoEof
