module Addition where

import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
         describe "Addition" $ do
           it "1 + 1 greater than 1" $ do
             ((1 :: Int) + 1) > 1 `shouldBe` True
           it "2 + 2 is equal to 4" $ do
             ((2 :: Int) + 2) `shouldBe` 4
           it "10 / 4 is equal to (2,2)" $ do
             divideBy (10 :: Int) 4 `shouldBe` (2,2)
           it "10 * 4 is equal to 40" $ do
             multiplyBy (10 :: Int) 4 `shouldBe` 40
           it "x + 1 is always\
              \ greater than x" $ do
             property $ \x -> x + 1 > (x :: Int)

divideBy :: Integral a => a -> a -> (a,a)
divideBy num denom = go num denom 0
                     where go num' denom' acc
                               | num' < denom' = (acc, num')
                               | otherwise     = go (num' - denom') denom' (acc + 1)

multiplyBy :: (Eq a, Num a) => a -> a -> a
multiplyBy left right = go left right 0
                        where go 0 _ acc = acc
                              go l r acc = go (l - 1) right (acc + r)

oneThroughThree :: Gen Int
oneThroughThree = elements [1,2,3]

genBool :: Gen Bool
genBool = choose (True, False)

genBool' :: Gen Bool
genBool' = elements [True, False]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a' .. 'z']

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a,b)

genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
genEither = do
  a <- arbitrary
  b <- arbitrary
  elements [Left a, Right b]

genMaybe :: Arbitrary a => Gen (Maybe a)
genMaybe = do
  a <- arbitrary
  elements [Nothing, Just a]

genMaybe' :: Arbitrary a => Gen (Maybe a)
genMaybe' = do
  a <- arbitrary
  frequency [ (1, return Nothing)
            , (3, return $ Just a)]

prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x

runQc :: IO ()
runQc = quickCheck prop_additionGreater
