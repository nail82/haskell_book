module FakeDB where

import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
    [
      DbNumber 9001
     ,DbString "Hello World"
     ,DbNumber 42
     ,DbDate (UTCTime
              (fromGregorian 1911 5 1)
              (secondsToDiffTime 34123))
     ,DbDate (UTCTime
              (fromGregorian 1921 5 1)
              (secondsToDiffTime 34123))
    ]


filterString :: [DatabaseItem] -> [String]
filterString = foldr f []
    where f (DbString a) b = a : b
          f _ b = b

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = foldr f epoch
    where f (DbDate a) b = max a b
          f _ b = b
          epoch = UTCTime (fromGregorian 1 1 1) (secondsToDiffTime 0)
