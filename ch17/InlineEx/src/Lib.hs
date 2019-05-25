module Lib where

import Data.List (elemIndex)

-- Pg 702
added :: Maybe Integer
added = (+3) <$> (lookup (3 :: Integer) $ zip [1,2,3] [4,5,6])

y :: Maybe Integer
y = lookup (3 :: Integer) $ zip [1,2,3] [4,5,6]

z :: Maybe Integer
z = lookup (2 :: Integer) $ zip [1,2,3] [4,5,6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z

x :: Maybe Int
x = elemIndex (3 :: Int) [1,2,3,3,5]

y1 :: Maybe Int
y1 = elemIndex (4 :: Int) [1,2,3,4,5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> x <*> y1

summed :: Maybe Integer
summed =  sum <$> ((,) <$> y <*> z)

-- Pg 705
newtype Identity a = Identity a
    deriving (Eq, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
    pure  a = Identity a
    (<*>) (Identity f) (Identity a) = Identity (f a)

-- Pg 721
v = const <$> Just "Hello" <*> Just "World"

w = (,,,) <$> Just 90
    <*> Just "Tierness"
    <*> Just [1 ,2,3]
    <*> Just "bub"
