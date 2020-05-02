module Tests.MyQC (runQC) where

import Test.QuickCheck

import Ex5

type HHMM = String

genMin :: Char -> Char -> String
genMin m mm = m : mm : []

-- A list of valid minutes
minutes = [genMin m mm | m <- "012345", mm <- "0123456789"]

-- A list of valid hours
hours = fmap showHour [0..23]

-- A list of valid hh:mm Generators
allTimes = [timeF h m  | h <- hours, m <- minutes]

timeF :: String -> String -> Gen HHMM
timeF h m = return $ h <> ":" <> m

showHour :: Int -> String
showHour h
         | h < 10 = "0" <> show h
         | otherwise = show h

hhmmGen :: Gen HHMM
hhmmGen = oneof allTimes

runQC :: IO ()
runQC = do
  putStrLn "=>QuickCheck tests not implemented.  Stay tuned.<="
