module DummyClass where

class Sumthin a where
    s :: a -> a

instance Sumthin Int where
    s a = a

data FixMePls a =
      FixMe
    | Pls a
      deriving (Eq, Show)

instance Functor FixMePls where
    fmap _ FixMe = FixMe
    fmap g (Pls a) = Pls (g a)
