module Lib where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- Write Foldable instances
data Constant a b =
    Constant b
    deriving (Eq, Show)

data Two a b =
    Two a b
    deriving (Eq, Show)

data Three a b c =
    Three a b c
    deriving (Eq, Show)

data Three' a b =
    Three' a b b
    deriving (Eq, Show)

data Four' a b =
    Four' a b b b
    deriving (Eq, Show)

instance Foldable (Constant a) where
    foldr f z (Constant b) = f b z

instance Foldable (Two a) where
    foldr f z (Two _ b) = f b z

instance Foldable (Three a b) where
    foldr f z (Three _ _ c) = f c z

instance Foldable (Three' a) where
    foldMap f (Three' _ b b') = (f b) <> (f b')

instance Foldable (Four' a) where
    foldMap f (Four' _ b b' b'') = (f b) <> (f b') <> (f b'')


-- A filter function for Foldable in terms of foldMap
--   Applicative gives <*> and pure
--   Foldable gives foldMap
--   Monoid gives <> and mempty
filterF :: ( Applicative f
           , Foldable t
           , Monoid (f a))
          => (a -> Bool) -> t a -> f a
filterF f ta = foldMap go ta
    where go a
             | f a       = pure a
             | otherwise = mempty
-- Shorter version
-- filterF f = foldMap (\x -> if f x then pure x else mempty)

-- I didn't think filterF should combine the filtered results, but
-- all the solutions I've see do so
