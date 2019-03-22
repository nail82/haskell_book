module Arith3Broken where

main :: IO()
main = do
  print (1 + 2 :: Integer)
  putStrLn (show (10 :: Integer))
  print (negate (-1) :: Integer)
  print ((+) 0 blah :: Integer)
  where blah = negate 1
