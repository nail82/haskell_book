{-# LANGUAGE FlexibleInstances #-}
module TypePrac where

data Doggies a =
               Husky a
               | Mastiff a
    deriving (Eq, Show)


data Manufacturer =
                  Mini
                  | Mazda
                  | Tata
    deriving (Eq, Show)

data Price =
    Price Integer deriving (Eq, Show)

data Size =
          Size Integer deriving (Eq, Show)

data Airline =
             PapuAir
             | CatapultsR'Us
             | Taca
    deriving (Eq, Show)


data Vehicle =
             Car Manufacturer Price
             | Plane Airline Size
    deriving (Eq, Show)

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars vs = map isCar vs

getManu :: Vehicle -> Manufacturer
getManu (Car m _) = m

-- data <type constructor> = <data constructor>
data Example = MakeExample Int deriving Show

class TooMany a where
    tooMany :: a -> Bool

instance TooMany Int where
    tooMany n = n > 42

instance TooMany [a] where
    tooMany xs = (length xs) > 5

-- newtypes can be defined as instances of type classes,
-- type aliases cannot

newtype Goats = Goats Int deriving (Eq, Show)

-- Can say this:
--
-- newtype Goats = Goats Int deriving (Eq, Show, TooMany)
--
-- if we use the pragma {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- and we've defined and instance of TooMany for the underlying
-- type (Int, in this case)

instance TooMany Goats where
    tooMany (Goats n) = n > 42

type Dogs = Int

--Doesn't work
--instance TooMany Dogs where
--    tooMany d = d > 42

instance TooMany (Int, String) where
    tooMany (n, s) = length s > n

instance (Num a, TooMany a) => TooMany (a, a) where
    tooMany (n1, n2) = tooMany (n1 + n2)

data BigSmall =
              Big Bool
              | Small Bool
              deriving (Eq, Show)

data Fiction = Fiction deriving Show
data NonFiction = NonFiction deriving Show

data BookType = FictionBook Fiction
              | NonFictionBook NonFiction
                deriving Show

type AuthorName = String

data Author = Author (AuthorName, BookType) deriving Show
