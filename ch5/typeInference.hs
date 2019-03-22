module TypeInference1 where

f :: Num a => a -> a -> a
f x y = x + y + 3

myConcat :: [Char] -> [Char]
myConcat x = x ++ " yo"

myTake :: Int -> [Char]
myTake x = take x "hey you"

bigNum = (^) 5 $ 10
wahoo = bigNum $ 10
