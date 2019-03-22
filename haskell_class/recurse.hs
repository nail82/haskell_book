mc91 :: Integral a => a -> a
mc91 n
     | n > 100 = n - 10
     | otherwise = mc91 . mc91 $ n + 11


mapMc91 :: Integral a => [a] -> [a]
mapMc91 [] = []
mapMc91 (x:xs) = (mc91 x) : (mapMc91 xs)
