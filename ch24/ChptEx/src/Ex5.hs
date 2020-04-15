{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
-- Log file parser
module Ex5 where

import Text.Trifecta
import Text.RawString.QQ

commentExample :: String
commentExample = "-- wheee a comment"

logExample :: String
logExample = "08:00 Breakfast"

dateExample :: String
dateExample = "# 2025-02-07 -- dates not nececessarily sequential"

twoLine :: String
twoLine = [r|
08:00 Breakfast -- should I try skippin bfast?
09:00 Bumped head, passed out
|]
