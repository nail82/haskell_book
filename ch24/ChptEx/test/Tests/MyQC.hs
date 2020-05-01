module Tests.MyQC (runQC) where

import Test.QuickCheck

genMin :: Char -> Char -> Gen String
genMin m mm = return $ m : mm : []

minutes = [genMin m mm | m <- "012345", mm <- "0123456789"]

minuteGen :: Gen String
minuteGen = oneof minutes

runQC :: IO ()
runQC = putStrLn "=>QuickCheck tests not implemented.  Stay tuned.<="
