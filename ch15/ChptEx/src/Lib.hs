module Lib where

import Data.Monoid
import Test.QuickCheck hiding (Failure, Success)

-- | Data Types

data Trivial = Trivial deriving (Eq, Show)

newtype Identity a = Identity a deriving (Eq, Show)

data Two a b = Two a b deriving (Eq, Show)

data Three a b c = Three a b c deriving (Eq, Show)

data Four a b c d = Four a b c d deriving (Eq, Show)

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

data Or a b =
    Fst a
  | Snd b deriving (Eq, Show)

newtype Combine a b =
    Combine { unCombine :: (a -> b) }

newtype Comp a =
    Comp { unComp :: (a -> a) }

data Validation a b =
    Failure a | Success b
    deriving (Eq, Show)


-- | Instances

instance Semigroup Trivial where
    (<>) _ _ = Trivial

instance Monoid Trivial where
    mempty = Trivial
    mappend = (<>)

instance Arbitrary Trivial where
    arbitrary = return Trivial

instance Semigroup a => Semigroup (Identity a) where
    (<>) (Identity a) (Identity b) = Identity (a <> b)

instance Monoid a => Monoid (Identity a) where
    mempty = Identity mempty
    mappend = (<>)

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

instance ( Arbitrary a
         , Arbitrary b
         , Arbitrary c
         , Arbitrary d
         , Semigroup a
         , Semigroup b
         , Semigroup c
         , Semigroup d)
    => Arbitrary (Four a b c d) where
        arbitrary = fourGen

instance Semigroup BoolConj where
    (<>) (BoolConj False) _ = (BoolConj False)
    (<>) _ (BoolConj False) = (BoolConj False)
    (<>) _ _                = (BoolConj True)

instance Arbitrary BoolConj where
    arbitrary = oneof [return (BoolConj True), return (BoolConj False)]

instance Semigroup BoolDisj where
    (<>) (BoolDisj True) _ = (BoolDisj True)
    (<>) _ (BoolDisj True) = (BoolDisj True)
    (<>) _ _               = (BoolDisj False)

instance Arbitrary BoolDisj where
    arbitrary = oneof [return (BoolDisj True), return (BoolDisj False)]

instance Semigroup (Or a b) where
    (<>) (Snd a) _       = (Snd a)
    (<>) (Fst _) (Snd b) = (Snd b)
    (<>) (Fst _) (Fst b) = (Fst b)

instance ( Arbitrary a
         , Arbitrary b) => Arbitrary (Or a b) where
    arbitrary = orGen

instance ( Semigroup b )
    => Semigroup (Combine a b) where
        (<>) (Combine f) (Combine g) = Combine ( f <> g )

-- instance (Semigroup b) => Arbitrary (Combine a b) where
--     arbitrary = _

instance Semigroup (Comp a) where
    (<>) (Comp f) (Comp g) = Comp (f . g)

instance (Semigroup a)
    => Semigroup (Validation a b) where
        (<>) (Failure a) (Failure a') = Failure (a <> a')
        (<>) (Success b) _            = Success b
        (<>) _ (Success b)            = Success b

instance (Arbitrary a, Arbitrary b, Semigroup a)
    => Arbitrary (Validation a b) where
        arbitrary = validationGen



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

fourGen :: (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Gen (Four a b c d)
fourGen = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  d <- arbitrary
  return (Four a b c d)

orGen :: (Arbitrary a, Arbitrary b) => Gen (Or a b)
orGen = do
  a <- arbitrary
  b <- arbitrary
  oneof [return (Fst a), return (Snd b)]

-- For repl testing
orGenIntString :: Gen (Or Int String)
orGenIntString = orGen

validationGen :: (Arbitrary a, Arbitrary b, Semigroup a) => Gen (Validation a b)
validationGen = do
  a <- arbitrary
  b <- arbitrary
  oneof [return (Failure a), return (Success b)]

-- For repl testing
validationStringInt :: Gen (Validation String Int)
validationStringInt = validationGen
