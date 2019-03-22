module MyFolds where

myOr :: [Bool] -> Bool
myOr [] = False
myOr xs = foldr (||) False xs

myAnd :: [Bool] -> Bool
myAnd [] = False
myAnd xs = foldr (&&) True xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\ x y -> f x || y) False

-- foldr f z [1,2,3]
-- 1 `f` (foldr f z [2,3])
--

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []
