module Lib where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data WhoCares a =
    ItDoesnt
        | Matter a
        | WhatThisIsCalled
    deriving (Eq, Show)

instance Functor WhoCares where
    fmap f (Matter a) = Matter (f a)
    fmap _ ItDoesnt   = ItDoesnt
    fmap _ WhatThisIsCalled = WhatThisIsCalled

data CountingBad a =
    Heisenberg Int a
    deriving (Eq, Show)

-- 16.10.01
newtype Identity a = Identity a
    deriving (Eq, Show)


instance Functor Identity where
    fmap f (Identity a) = Identity (f a)
