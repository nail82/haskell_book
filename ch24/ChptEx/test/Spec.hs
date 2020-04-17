{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import Data.ByteString (ByteString)
import Data.Time
import Text.RawString.QQ
import Text.Trifecta
import Test.Hspec
import Ex5

maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing

commentExample :: ByteString
commentExample = [r|-- wheee a comment
|]

logExample :: ByteString
logExample = "08:00 Breakfast"

dateExample :: ByteString
dateExample = "# 2025-02-07 -- dates not nececessarily sequential\n"

twoLine :: ByteString
twoLine = [r|
08:00 Breakfast -- should I try skippin bfast?
09:00 Bumped head, passed out
|]

twoWithDay :: ByteString
twoWithDay = [r|# 2020-03-21
08:00 Breakfast -- should I try skippin bfast?
09:00 Bumped head, passed out
|]

weeTime :: ByteString
weeTime = "08:00"

lateTime :: ByteString
lateTime = "23:01"

eventWithComment :: ByteString
eventWithComment = "bub, a dude -- comment"

eventWOComment :: ByteString
eventWOComment = "bub, a dude"

pb f s = parseByteString f mempty s

main :: IO ()
main = hspec $ do
         describe "Day header" $
               it "can parse a day header" $ do
                 let m = pb parseDayStamp dateExample
                     r' = maybeSuccess m
                 print m
                 r' `shouldBe` Just (fromGregorian 2025 2 7)
         describe "Early time stamp" $
               it "can parse an early time stamp" $ do
                 let m = pb parseTimeStamp weeTime
                     r' = maybeSuccess m
                 print m
                 r' `shouldBe` Just 28800
         describe "Late time stamp" $
               it "can parse a late time stamp" $ do
                 let m = pb parseTimeStamp lateTime
                     r' = maybeSuccess m
                 print m
                 r' `shouldBe` Just 82860
         describe "Event" $
               it "can parse an event with a comment" $ do
                 let  m = pb parseEvent eventWithComment
                      r' = maybeSuccess m
                 print m
                 r' `shouldBe` Just "bub, a dude "
         describe "Event" $
               it "can parse an event without a comment" $ do
                 let  m = pb parseEvent eventWOComment
                      r' = maybeSuccess m
                 print m
                 r' `shouldBe` Just "bub, a dude"
         describe "Log Entry" $
               it "can parse a single log entry" $ do
                  let m = pb parseLogEntry logExample
                      r' = maybeSuccess m
                  print m
                  r' `shouldBe` Just (LogEntry 28800 "Breakfast")
         describe "Log Entries" $
               it "can parse multiple log entries" $ do
                 let m = pb parseEntries twoLine
                     r' = maybeSuccess m
                 print m
                 r' `shouldBe` Just ([LogEntry 28800 "Breakfast "
                                     , LogEntry 32400 "Bumped head, passed out"])
         describe "Log Day" $
               it "can parse a day's logs" $ do
                 let m = pb parseLogDay twoWithDay
                     r' = maybeSuccess m
                 print m
                 r' `shouldBe` Just (LogDay
                                     (fromGregorian 2020 3 21) [
                                      LogEntry 28800 "Breakfast "
                                     , LogEntry 32400 "Bumped head, passed out"])
