module ChExMaybe where

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _       = True

isNothing :: Maybe a -> Bool
isNothing = not . isJust

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b _ Nothing  = b
mayybee _ f (Just a) = f a

-- This was my first intuition
fromMaybe :: a -> Maybe a -> a
fromMaybe a Nothing  = a
fromMaybe _ (Just a) = a

-- Using the catamorphism
fromMaybe' :: a -> Maybe a -> a
fromMaybe' a ma = mayybee a id ma

listToMaybe :: [a] -> Maybe a
listToMaybe []    = Nothing
listToMaybe (a:_) = Just a

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]

catMaybes :: [Maybe a] -> [a]
catMaybes []            = []
catMaybes (Just x:xs)   = x : catMaybes xs
catMaybes (Nothing:xs)  = catMaybes xs

-- My first intuition. Not very clean and it runs through the list twice (with any)
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe [] = Just []
flipMaybe l@((Just x):xs)
          | any isNothing l = Nothing
          | otherwise = Just $ x : go xs
          where go [] = []
                go ((Just y):ys) = y : go ys

-- From here: https://github.com/dwayne/haskell-programming/blob/master/ch12/Maybe.hs
-- Much cleaner.  Recursion is handled at the top function level.
flipMaybe' :: [Maybe a] -> Maybe [a]
flipMaybe' [] = Just []
flipMaybe' (Nothing:_) = Nothing
flipMaybe' (Just x:xs) =
    case flipMaybe' xs of
      Just ys -> Just (x:ys)
      Nothing -> Nothing
