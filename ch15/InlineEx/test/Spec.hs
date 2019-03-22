module Main where

import Test.QuickCheck
import Data.Monoid
import Control.Monad
import Pg598

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (a <> mempty)  == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (mempty <> a) == a


firstMappend :: First' a
             -> First' a
             -> First' a
firstMappend = mappend

type FirstMappend =
    First' String
    -> First' String
    -> First' String
    -> Bool

type FirstId =
    First' String -> Bool

instance Arbitrary a => Arbitrary (First' a) where
    arbitrary =
        frequency [(1, return (First' Nada))
                  ,(1, onlyGen)
                  ]

-- This part was tricky.  I tried to set the signature as
-- onlyGen :: Arbitrary a => Gen (First' (Optional a)),
-- but note the type of return (First' (Only 1)) is actually
-- (Monad m, Num a) => m (First' a)
--
-- It makes sense in one way that First' is a newtype
-- which wraps a single type
onlyGen :: Arbitrary a => Gen (First' a)
onlyGen = do
  a <- arbitrary
  return (First' (Only a))

onlyGenInt :: Gen (First' Int)
onlyGenInt = onlyGen


main :: IO ()
main = do
   quickCheck (monoidAssoc :: FirstMappend)
   quickCheck (monoidLeftIdentity :: FirstId)
   quickCheck (monoidRightIdentity :: FirstId)
