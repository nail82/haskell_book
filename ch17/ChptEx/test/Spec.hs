module Main where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Data.Monoid
import ChLib

main :: IO ()
main = do
  putStrLn("\n==> Four' <==")
  let trigger5 :: Four' (Sum Int, String, Sum Int)
                  (Int, String, Int)
      trigger5 = undefined
  quickBatch $ functor trigger5
  quickBatch $ applicative trigger5

  putStrLn("\n==> Four <==")
  let trigger4 :: Four (Sum Int, String, Sum Int)
                  (Sum Int, String, Sum Int)
                  (Sum Int, String, Sum Int)
                  (Int, String, Int)
      trigger4 = undefined
  quickBatch $ functor trigger4
  quickBatch $ applicative trigger4

  putStrLn("\n==> Three' <==")
  let trigger3 :: Three' (Sum Int, String, Product Int)
                  (Int, String, Int)
      trigger3 = undefined
  quickBatch $ functor trigger3
  quickBatch $ applicative trigger3

  putStrLn("\n==> Three <==")
  let trigger2 :: Three (Sum Int, String, Product Int)
                  (Sum Int, String, Product Int)
                  (Int, String, Int)
      trigger2 = undefined
  quickBatch $ functor trigger2
  quickBatch $ applicative trigger2

  putStrLn("\n==> Two <==")
  let trigger1 :: Two (Sum Int, String, Product Int) (Int, String, Int)
      trigger1 = undefined
  quickBatch $ functor trigger1
  quickBatch $ applicative trigger1

  putStrLn("\n==> Pair <==")
  let trigger :: Pair (Int, String, Int)
      trigger = undefined
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
