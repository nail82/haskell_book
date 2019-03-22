module ChExNaturalNumber where

data Nat =
         Zero
         | Succ Nat
    deriving (Eq, Show)


natToInteger :: Nat -> Integer
natToInteger Zero     = 0
natToInteger (Succ n) = 1 + natToInteger n


-- Could've pattern matched in go
integerToNat :: Integer -> Maybe Nat
integerToNat i
             | i < 0     = Nothing
             | otherwise = Just $ go i
             where go i1
                       | i1 == 0   = Zero
                       | otherwise = Succ $ go $ i1 - 1
