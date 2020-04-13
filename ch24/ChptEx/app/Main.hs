{-# LANGUAGE OverloadedStrings #-}
module Main where

import Text.Trifecta
import Ex1

justCore :: String
justCore = "1.2.3"

coreAndRelease :: String
coreAndRelease = "1.2.3-abc.456"

coreAndBuild :: String
coreAndBuild = "1.2.3+build.456"

coreReleaseBuild :: String
coreReleaseBuild = "1.2.3-abc.456+build.789"

p f s = parseString f mempty s


main :: IO ()
main = do
  print $ p semVerParser justCore
  print $ p semVerParser coreAndRelease
  print $ p semVerParser coreAndBuild
  print $ p semVerParser coreReleaseBuild
