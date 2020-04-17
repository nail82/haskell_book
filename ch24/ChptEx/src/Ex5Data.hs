{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Ex5Data where

import Text.RawString.QQ

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

dayWithLeadingWhitespace :: String
dayWithLeadingWhitespace = [r|
# 2020-03-21 -- a comment
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
