module ExPg178 where

data TisAnInteger =
    TisAn Integer deriving Show

instance Eq TisAnInteger where
    (==) (TisAn a) (TisAn a') = a == a'

data TwoIntegers =
    Two Integer Integer deriving Show

instance Eq TwoIntegers where
    (==) (Two a1 a2) (Two a1' a2') =
        a1 == a1' && a2 == a2'


data StringOrInt =
      TisAnInt Int
    | TisAString String
      deriving Show

instance Eq StringOrInt where
    (==) (TisAnInt a) (TisAnInt a') = a == a'
    (==) (TisAString a) (TisAString a') = a == a'
    (==) _ _ = False

data Pair a =
    Pair a a
    deriving Show

instance Eq a => Eq (Pair a) where
    (==) (Pair a b) (Pair a' b') =
        a == a' && b == b'

data Tuple a b =
    Tuple a b
    deriving Show

instance (Eq a, Eq b) => Eq (Tuple a b) where
    (==) (Tuple a b) (Tuple a' b') =
        a == a' && b == b'

data Which a =
      ThisOne a
    | ThatOne a
    deriving Show

instance Eq a => Eq (Which a) where
    (==) (ThisOne a) (ThisOne a') = a == a'
    (==) (ThatOne a) (ThatOne a') = a == a'
    (==) _ _ = False

data EitherOr a b =
      Hello a
    | Goodbye b
      deriving Show

instance (Eq a, Eq b) => Eq (EitherOr a b) where
    (==) (Hello a) (Hello a') = a == a'
    (==) (Goodbye b) (Goodbye b') = b == b'
    (==) _ _ = False
