module ChExEither where

-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
myL :: Either c d -> [c] -> [c]
myL (Left c)  cs = c : cs
myL (Right _) cs = cs

myR :: Either c d -> [d] -> [d]
myR (Left _)  ds = ds
myR (Right d) ds = d : ds

lefts' :: [Either a b] -> [a]
lefts' eas = foldr myL [] eas

rights' :: [Either a b] -> [b]
rights' ebs = foldr myR [] ebs

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' es = (lefts' es, rights' es)

either' :: (a -> c)
        -> (b -> c)
        -> Either a b
        -> c
either' fa _ (Left a)  = fa a
either' _ fb (Right b) = fb b

-- My first intuition
eitherMaybe'' :: (b -> c)
              -> Either a b
              -> Maybe c
eitherMaybe'' _  (Left _)  = Nothing
eitherMaybe'' fb (Right b) = Just $ either' id fb (Right b)

-- From https://github.com/dwayne/haskell-programming/blob/master/ch12/Either.hs
-- Clever use of partial application
-- const :: a -> b -> a
eitherMaybe''' :: (b -> c)
              -> Either a b
              -> Maybe c
eitherMaybe''' fb = either' (const Nothing) (Just . fb)
