module Lib where

import Data.Monoid
import Test.QuickCheck

-- | Data Types

data Trivial = Trivial deriving (Eq, Show)

newtype Identity a = Identity a deriving (Eq, Show)

data Two a b = Two a b deriving (Eq, Show)

data Three a b c = Three a b c deriving (Eq, Show)

data Four a b c d = Four a b c d deriving (Eq, Show)

newtype BoolConj =
    BoolConj Bool



-- | Instances

instance Semigroup Trivial where
    (<>) _ _ = Trivial

instance Arbitrary Trivial where
    arbitrary = return Trivial

instance Semigroup a => Semigroup (Identity a) where
    (<>) (Identity a) (Identity b) = Identity (a <> b)

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = identityGen

instance ( Semigroup a
         , Semigroup b)
    => Semigroup (Two a b) where
        (<>) (Two a b) (Two a' b') =
            Two (a <> a') (b <> b')

instance ( Arbitrary a
         , Arbitrary b
         , Semigroup a
         , Semigroup b) => Arbitrary (Two a b) where
    arbitrary = twoGen

instance ( Semigroup a
         , Semigroup b
         , Semigroup c)
    => Semigroup (Three a b c) where
        (<>) (Three a b c) (Three a' b' c') =
            Three (a <> a') (b <> b') (c <> c')

instance ( Arbitrary a
         , Arbitrary b
         , Arbitrary c
         , Semigroup a
         , Semigroup b
         , Semigroup c)
    => Arbitrary (Three a b c) where
        arbitrary = threeGen

instance ( Semigroup a
         , Semigroup b
         , Semigroup c
         , Semigroup d)
    => Semigroup (Four a b c d) where
        (<>) (Four a b c d) (Four a' b' c' d') =
            Four (a <> a') (b <> b') (c <> c') (d <> d')

-- | Generators

trivialGen :: Gen Trivial
trivialGen = do
  return Trivial

identityGen :: Arbitrary a => Gen (Identity a)
identityGen = do
  a <- arbitrary
  return (Identity a)

twoGen :: (Arbitrary a, Arbitrary b) => Gen (Two a b)
twoGen = do
  a <- arbitrary
  b <- arbitrary
  return (Two a b)

threeGen :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (Three a b c)
threeGen = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (Three a b c)
