module Lib where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

half :: Fractional a => a -> a
half x = x / 2

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
    snd $ foldr go (Nothing, True) xs
        where go _ status@(_, False) = status
              go y (Nothing, t)      = (Just y, t)
              go y (Just x,  t)      = (Just y, x >= y)


funcAssociative :: (Eq a, Integral a) => (a -> a -> a) -> a -> a -> a -> Bool
funcAssociative f x y z =
    x `f` (y `f` z) == (x `f` y) `f` z

funcCommutative :: (Eq a, Integral a) => (a -> a -> a) -> a -> a -> Bool
funcCommutative f x y =
    x `f` y == y `f` x
