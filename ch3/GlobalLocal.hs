module GlobalLocal where

topLevelFunction :: Integer -> Integer
topLevelFunction x = x + woot + topLevelValue
    where woot :: Integer
          woot =10

topLevelValue :: Integer
topLevelValue = 3

addExclaim :: String -> String
addExclaim s = s ++ "!"

getY :: [Char] -> Char
getY s = head $ dropWhile isNotY s
    where
      isNotY x = x /= 'y'
