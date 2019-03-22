module Exercises where

-- Two recursive calls
fibonacci :: Integral a => a -> a
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = (fibonacci (n - 1)) + (fibonacci (n - 2))

-- Linear
fibonacci' :: Integral a => a -> a
fibonacci' n = go 0 1 n
               where
                 go _ 1   1   = 1
                 go v2 v1 2   = v2 + v1
                 go v2 v1 cnt = go v1 (v2+v1) (cnt - 1)

-- 1 1 2 3 5 8 13
-- fib 0         = 1
-- fib 1         = 1
-- fib 2 = 1 + 1 = 2
-- fib 3 = 1 + 2 = 3
-- fib 4 = 2 + 3 = 5

mc91 :: Integral a => a -> a
mc91 n
     | n > 100   = n - 10
     | otherwise = (mc91 . mc91) (n + 11)
