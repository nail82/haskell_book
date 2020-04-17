{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Main where

import Text.Trifecta
import Ex1
import Ex2
import Ex4

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
