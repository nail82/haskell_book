module MyMax where

myListMax :: Ord a => [a] -> Maybe a
myListMax [] = Nothing
myListMax (x:y:xs) =
    if x > y
    then myListMax (x:xs)
    else myListMax (y:xs)
myListMax (x:_) = Just x
