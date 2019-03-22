module DataTypeTry where

import Data.List.NonEmpty

data P = Prefix Int String
       deriving (Show)


data Q = Int :!!: String
       deriving (Show)
