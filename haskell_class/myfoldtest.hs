module MyFoldTest where


myfold :: Int -> Int -> Int
myfold x y
    | y > 3 = y
    | otherwise = x + y
