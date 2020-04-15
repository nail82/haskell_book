{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Main where

import Text.Trifecta
import Text.RawString.QQ
import Ex1
import Ex2
import Ex4
import Ex5

justCore :: String
justCore = "1.2.3"

coreAndRelease :: String
coreAndRelease = "1.2.3-abc.456"

coreAndBuild :: String
coreAndBuild = "1.2.3+build.456"

coreReleaseBuild :: String
coreReleaseBuild = "1.2.3-abc.456+build.789"

dashDelimited :: String
dashDelimited = "123-456-7890"

tenDigits :: String
tenDigits = "1234567890"

noDashes :: String
noDashes = "1234567890"

oldWay :: String
oldWay = "(123) 456-7890"

countryCode :: String
countryCode = "1-123-456-7890"

countrySpaceDelimited :: String
countrySpaceDelimited = "1 123 456 7890"

exampleData :: String
exampleData = [r|
-- wheee a comment

# 2025-02-05
08:00 Breakfast
09:00 Sanitizing moisture collector
11:00 Exercising in high-grav gym
12:00 Lunch
13:00 Programming
17:00 Commuting home in rover
17:30 R&R
19:00 Dinner
21:00 Shower
21:15 Read
22:00 Sleep

# 2025-02-07 -- dates not nececessarily sequential
08:00 Breakfast -- should I try skippin bfast?
09:00 Bumped head, passed out
13:36 Wake up, headache
13:37 Go to medbay
13:40 Patch self up
13:45 Commute home for rest
14:15 Read
21:00 Dinner
21:15 Read
22:00 Sleep
|]


p f s = parseString f mempty s

main :: IO ()
main = do
  -- Ex 1
  print $ p semVerParser justCore
  print $ p semVerParser coreAndRelease
  print $ p semVerParser coreAndBuild
  print $ p semVerParser coreReleaseBuild
  -- Ex 2
  print $ p base10Integer tenDigits
  -- Ex 4
  print $ p parsePhone dashDelimited
  print $ p parsePhone noDashes
  print $ p parsePhone oldWay
  print $ p parsePhone countryCode
  print $ p parsePhone countrySpaceDelimited
