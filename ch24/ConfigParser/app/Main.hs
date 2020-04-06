{-# LANGUAGE OverloadedStrings #-}
module Main where

import Text.Trifecta
import Data.Ini

main :: IO ()
main = do
  print $ parseByteString parseHeader mempty headerEx
