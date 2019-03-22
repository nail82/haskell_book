module FoldPrac where

g :: [Char] -> [Char] -> [Char]
g a b = (take 3 a ++ b)

h :: [Char] -> [Char] -> [Char]
h b a = (take 3 a ++ b)


fibs = 1 : scanl (+) 1 fibs

fibN x = fibs !! x


fibs' = take 20 (1 : scanl (+) 1 fibs)


-- Recursive factorial function
fac :: Integer -> Integer
fac 0 = 1
fac 1 = 1
fac n = n * fac (n-1)

-- Scanl factorial function
fac' :: Integer -> [Integer]
fac' = undefined

u = foldr max ' ' "fear is the little death"

-- ["Pizza", "Apple", "Banana"] -> "PizAppBan"
foods = ["Pizza", "Apple", "Banana"]
first3 :: [String] -> String
first3 xs = foldr (\a b -> take 3 a ++ b) "" xs
