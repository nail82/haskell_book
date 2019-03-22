module Pg598 where

data Optional a =
    Nada
    | Only a
    deriving (Eq, Show)


instance Monoid a => Monoid (Optional a) where
    mempty  = Nada
    mappend = (<>)

instance Semigroup a => Semigroup (Optional a) where
    (<>) (Only a1) (Only a2) = Only (a1 <> a2)
    (<>) _ (Only a) = Only a
    (<>) (Only a) _ = Only a
    (<>) Nada Nada = Nada

newtype First' a =
    First' { getFirst' :: Optional a}
    deriving (Eq, Show)

instance Semigroup (First' a) where
    (<>) (First' (Only a)) _ = First' (Only a)
    (<>) (First' Nada) (First' (Only a)) = First' (Only a)
    (<>) (First' Nada) (First' Nada) = First' Nada

instance Monoid (First' a) where
    mempty = First' Nada
    mappend = (<>)

itWorks :: IO ()
itWorks = putStrLn("Hello")
