module Main where

import Test.QuickCheck
import Test.QuickCheck.All
import Test.QuickCheck.Gen (oneof)
import Lib
import Data.Char
import Data.List

halfIdentity = (*2) . half

prop_halfIdentity :: Property
prop_halfIdentity =
    forAll (arbitrary :: (Gen Float))
           (\v -> halfIdentity v == v)

prop_addOne :: Property
prop_addOne =
    forAll (arbitrary :: (Gen Int))
           (\x -> x + 1 > x)

prop_listOrdered :: Property
prop_listOrdered =
    forAll (orderedList :: Gen [Int])
           (\xs -> listOrdered xs)

prop_plusAssociative :: Property
prop_plusAssociative =
    forAll (arbitrary :: (Gen Int))
           (\x y z -> funcAssociative (+) x y z)

prop_plusCommutative :: Int -> Int -> Bool
prop_plusCommutative x y = x + y == y + x

-- fails
prop_powCommutative :: (NonNegative Int)
                    -> (NonNegative Int)
                    -> Bool
prop_powCommutative (NonNegative x) (NonNegative y)
    = x ^ y == y ^ x

-- fails
prop_powAssociative :: (NonNegative Int)
                    -> (NonNegative Int)
                    -> (NonNegative Int)
                    -> Bool
prop_powAssociative (NonNegative x) (NonNegative y) (NonNegative z)
    = x ^ (y ^ z) == (x ^ y) ^ z

prop_QuotRem :: (NonZero Int)
             -> (NonZero Int)
             -> Bool
prop_QuotRem (NonZero x) (NonZero y) = (quot x y) * y + (rem x y) == x

prop_DivMod :: (NonZero Int)
            -> (NonZero Int)
            -> Bool
prop_DivMod (NonZero x) (NonZero y) = (div x y) * y + (mod x y) == x

prop_ListReverse :: Property
prop_ListReverse =
    forAll (arbitrary :: (Gen [Int]))
           (\xs -> (reverse . reverse) xs == xs)

prop_FuncApp :: Int -> Bool
prop_FuncApp x = (f $ x) == f x
    where f = (+) 3

-- fails
prop_foldrConcat :: [Int] -> [Int] -> Bool
prop_foldrConcat xs ys = (f xs ys) == (f' xs ys)
    where f  = foldr (:)
          f' = (++)

-- fails
prop_takeLength :: Int -> [Int] -> Bool
prop_takeLength n xs = length (take n xs) == n

prop_readShow :: Int -> Bool
prop_readShow n = (read (show n)) == n

square :: Float -> Float
square x = x * x

squareId = square . sqrt

-- fails.  Probably some way to do this with a NonNegative modifier
prop_squareId :: Float -> Bool
prop_squareId x
    | x < 0 = True
    | otherwise = (squareId x) == x

twice f = f . f

fourTimes = twice . twice

capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (x:xs) = (toUpper x) : xs

prop_idempotentCapitalize :: String -> Bool
prop_idempotentCapitalize x =
    (capitalizeWord x == twice capitalizeWord x)
    &&
    (capitalizeWord x == fourTimes capitalizeWord x)

prop_idempotentSort :: [Int] -> Bool
prop_idempotentSort xs =
    (sort xs == twice sort xs)
    &&
    (sort xs == fourTimes sort xs)

data Fool =
    Fulse
  | Frue
  deriving (Eq, Show)

foolGen :: Gen Fool
foolGen = oneof [return Fulse, return Frue]

instance Arbitrary Fool where
    arbitrary = foolGen

weigtedFoolGen :: Gen Fool
weigtedFoolGen =
    frequency
    [
      (1, return Frue)
    , (2, return Fulse)
    ]





main :: IO ()
main = quickCheck (prop_FuncApp
                           .&. prop_readShow
                           .&. prop_idempotentCapitalize
                           .&. prop_idempotentSort)

       -- (prop_addOne
       --   .&. prop_halfIdentity
       --   .&. prop_listOrdered
       --   .&. prop_plusAssociative)
