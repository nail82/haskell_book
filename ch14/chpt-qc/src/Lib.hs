module Lib where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

half :: Fractional a => a -> a
half x = x / 2

-- Linear search to determine if a list is sorted. From pg 575
-- Uses a tuple to fold the list.
listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
    snd $ foldr go (Nothing, True) xs
        where go _ status@(_, False) = status
              go y (Nothing, t)      = (Just y, t)
              go y (Just x,  _)      = (Just y, x >= y)


funcAssociative :: (Eq a, Integral a) => (a -> a -> a) -> a -> a -> a -> Bool
funcAssociative f x y z =
    x `f` (y `f` z) == (x `f` y) `f` z

funcCommutative :: (Eq a, Integral a) => (a -> a -> a) -> a -> a -> Bool
funcCommutative f x y =
    x `f` y == y `f` x
