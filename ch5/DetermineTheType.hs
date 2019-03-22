{-# LANGUAGE NoMonomorphismRestriction #-}

module DetermineTheType where

example = 1

x = 5
y = x + 5
z y = y * 10

functionC :: (Ord a) => a -> a -> Bool
functionC x' y' = x' > y'


co :: (b -> c) -> (a -> b) -> a -> c
co bToC aToB a = bToC $ aToB a
