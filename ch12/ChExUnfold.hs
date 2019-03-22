module ChExUnfold where

myIterate :: (a -> a) -> a -> [a]
myIterate f x = x : myIterate f (f x)

myRepeat :: a -> [a]
myRepeat = myIterate id

myUnfoldr :: (b -> Maybe (a,b)) -> b -> [a]
myUnfoldr f b =
    case (f b) of
      Nothing      -> []
      Just (a, b1) -> a : myUnfoldr f b1

g :: Int -> Maybe (Int, Int)
g 0 = Nothing
g x = Just (x, x-1)

-- Unfold the sum of the first N integers
g' :: Int -> Maybe (Int, Int)
g' 0 = Nothing
g' x = undefined

betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr (\b -> Just (b, f b)) x
