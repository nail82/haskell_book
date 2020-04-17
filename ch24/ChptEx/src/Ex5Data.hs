{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Ex5Data where

import Text.RawString.QQ

commentExample :: String
commentExample = [r|-- wheee a comment
|]

logExample :: String
logExample = "08:00 Breakfast"

dateExample :: String
dateExample = "# 2025-02-07 -- dates not nececessarily sequential\n"

twoLine :: String
twoLine = [r|
08:00 Breakfast -- should I try skippin bfast?
09:00 Bumped head, passed out
|]

twoWithDay :: String
twoWithDay = [r|# 2020-03-21
08:00 Breakfast -- should I try skippin bfast?
09:00 Bumped head, passed out
|]

threeWithDay :: String
threeWithDay = [r|# 2020-03-21
08:00 Breakfast
09:00 Bumped head, passed out
10:00 workout
|]

threeWithDayComment :: String
threeWithDayComment = [r|# 2020-03-21 -- a comment
08:00 Breakfast
09:00 Bumped head, passed out
10:00 workout -- bfast comment
|]

weeTime :: String
weeTime = "08:00"

lateTime :: String
lateTime = "23:01"

eventWithComment :: String
eventWithComment = "bub, a dude -- comment"

eventWOComment :: String
eventWOComment = "bub, a dude"
