module ExPg657 where

-- Campaign for North Africa

-- a = (+1) $ read "[1]" :: Int
-- expected [2]
a :: [Int]
a = fmap (+1) (read "[1]" :: [Int])


-- b = (++ "lol") (Just ["Hi,", "Hello"])
-- expected Just ["Hi,lol", "Hellolol"]
b :: Maybe [String]
b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

-- c = (*2) (\x -> x - 2)
-- expected c 1
-- -2
c :: Int -> Int
c = (* (2 :: Int)) . (\x -> x - 2)

-- expected d 0
-- 1[0,1,2,3]
--d =
--    ((return '1' ++) . show)
--    (\x -> [x, 1..3])
d :: (Show a, Enum a, Num a) => a -> [Char]
d = (return '1' ++) . show . (\x -> [x, 1..3])



e :: IO Integer
e = undefined
--e = let ioi = readIO "1" :: IO Integer
--        changed = read ("123" ++) show ioi
--    in (*3) changed

-- 3693

main :: IO ()
main = undefined
