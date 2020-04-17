{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import Data.ByteString (ByteString)
import Data.Time
import Text.RawString.QQ
import Text.Trifecta
import Test.Hspec
import Ex5
import Ex5Data

maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing

pb f s = parseString f mempty s

main :: IO ()
main = hspec $ do
         describe "Skip Comment" $
               it "can skip comment lines" $ do
                 let m = pb skipComments commentExample
                     r' = maybeSuccess m
                 print m
                 r' `shouldBe` Just ()
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
                 r' `shouldBe` Just "bub, a dude"
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
                 let m = pb (some parseLogEntry) twoLine
                     r' = maybeSuccess m
                 print m
                 r' `shouldBe` Just ([LogEntry 28800 "Breakfast"
                                     , LogEntry 32400 "Bumped head, passed out"])
         describe "Log Day" $
               it "can parse a day's logs" $ do
                 let m = pb parseLogDay threeWithDay
                     r' = maybeSuccess m
                 print m
                 r' `shouldBe` Just (LogDay
                                     (fromGregorian 2020 3 21) [
                                      LogEntry 28800 "Breakfast"
                                     , LogEntry 32400 "Bumped head, passed out"
                                     , LogEntry 36000 "workout"])
         describe "Log Day with comment" $
               it "can parse a day's logs with a comment" $ do
                 let m = pb parseLogDay threeWithDayComment
                     r' = maybeSuccess m
                 print m
                 r' `shouldBe` Just (LogDay
                                     (fromGregorian 2020 3 21) [
                                      LogEntry 28800 "Breakfast"
                                     , LogEntry 32400 "Bumped head, passed out"
                                     , LogEntry 36000 "workout"])
         describe "Log Day with leading whitespace" $
               it "can parse a day's logs with leading whitespace" $ do
                 let m = pb parseLogDay dayWithLeadingWhitespace
                     r' = maybeSuccess m
                 print m
                 r' `shouldBe` Just (LogDay
                                     (fromGregorian 2020 3 21) [
                                      LogEntry 28800 "Breakfast"
                                     , LogEntry 32400 "Bumped head, passed out"
                                     , LogEntry 36000 "workout"])
