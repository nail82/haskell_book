module Tests.MyQC (runQC) where

import Test.QuickCheck

genMin :: Char -> Char -> String
genMin m mm = m : mm : []

minutes = [genMin m mm | m <- "012345", mm <- "0123456789"]

hours = fmap showHour validHours

validHours :: [Int]
validhours = [0..23]

showHour :: Int -> String
showHour h
         | h < 10 = "0" <> show h
         | otherwise = show h


runQC :: IO ()
runQC = putStrLn "=>QuickCheck tests not implemented.  Stay tuned.<="
