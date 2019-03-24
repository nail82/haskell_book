module Main where

import Test.QuickCheck
import Lib

semiGroupAssoc :: (Eq m, Semigroup m) =>
                  m -> m -> m -> Bool
semiGroupAssoc a b c =
    ((a <> b) <> c) == (a <> (b <> c))

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = mempty <> a == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = a <> mempty == a



-- | Trivial

type TrivialAssoc =
    Trivial -> Trivial -> Trivial -> Bool


-- | Identity

identityGenString :: Gen (Identity String)
identityGenString = identityGen

type IdentityAssoc =
    (Identity String) -> (Identity String) -> (Identity String) -> Bool

-- | Two

twoGenString :: Gen (Two String String)
twoGenString = twoGen

type TwoAssoc =
    (Two String String) -> (Two String String) -> (Two String String) -> Bool

type ThreeAssoc =
    (Three String String String)
    -> (Three String String String)
    -> (Three String String String)
    -> Bool

type FourAssoc =
    (Four String String String String)
    -> (Four String String String String)
    -> (Four String String String String)
    -> Bool

type BoolConjAssoc =
    BoolConj -> BoolConj -> BoolConj -> Bool

type BoolDisjAssoc =
    BoolDisj -> BoolDisj -> BoolDisj -> Bool

type OrAssoc =
    (Or Int String) -> (Or Int String) -> (Or Int String) -> Bool

type ValidationAssoc =
    (Validation String Int) -> (Validation String Int) -> (Validation String Int) -> Bool


main :: IO ()
main = do
  -- Semigroups
  quickCheck(semiGroupAssoc :: TrivialAssoc)
  quickCheck(semiGroupAssoc :: IdentityAssoc)
  quickCheck(semiGroupAssoc :: TwoAssoc)
  quickCheck(semiGroupAssoc :: ThreeAssoc)
  quickCheck(semiGroupAssoc :: FourAssoc)
  quickCheck(semiGroupAssoc :: BoolConjAssoc)
  quickCheck(semiGroupAssoc :: BoolDisjAssoc)
  quickCheck(semiGroupAssoc :: OrAssoc)
  quickCheck(semiGroupAssoc :: ValidationAssoc)
  -- Monoids
  quickCheck(monoidLeftIdentity  :: Trivial -> Bool)
  quickCheck(monoidRightIdentity :: Trivial -> Bool)
  quickCheck(monoidLeftIdentity  :: Identity String -> Bool)
  quickCheck(monoidRightIdentity :: Identity String -> Bool)
