module Pg836 where

import Data.Monoid

sum :: (Foldable t, Num a) => t a -> a
sum ta = getSum $ foldMap Sum ta

product :: (Foldable t, Num a) => t a -> a
product ta = getProduct $ foldMap Product ta

elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem v = foldr (\v' z -> v == v' || z) False

-- Got some help from here: https://github.com/mukeshsoni/haskell-programming-book
-- Clever use of Maybe to get the ball rolling
doCompare :: (Ord a) => (a -> a -> Bool) -> a -> Maybe a -> Maybe a
doCompare _ x Nothing = Just x
doCompare comp x (Just x')
       | x `comp` x' = Just x
       | otherwise   = Just x'

minimum :: (Foldable t, Ord a) => t a -> Maybe a
minimum = foldr (doCompare (<)) Nothing

maximum :: (Foldable t, Ord a) => t a -> Maybe a
maximum = foldr (doCompare (>)) Nothing

null' :: (Foldable t) => t a -> Bool
null' ta = (length' ta == 0)
-- or
-- null' = foldr (\_ _ -> False) True

length' :: (Foldable t) => t a -> Int
length' = foldr (\_ z -> z + 1) 0

toList :: (Foldable t) => t a -> [a]
toList = foldr (\v z -> v : z) []

-- In terms of foldMap
fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

-- In terms of foldr
foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (\a z -> (f a) <> z) mempty
