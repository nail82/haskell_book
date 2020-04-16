{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
-- Log file parser
module Ex5 where

import Control.Applicative
import Data.ByteString (ByteString)
import Text.Trifecta
import Text.RawString.QQ
import Data.Char (digitToInt)
import Data.Time


-- Going to have to look ahead in the log to pick up the next event time
-- Set eventDuration to -1 if we don't find a time on the next line
-- Filter the negative values when aggregating
data LogEntry = LogEntry {
      sinceMid :: DiffTime,
      logEvent :: String
    }

data LogDay = LogDay Day [LogEntry]

hhMM :: FormatTime t => t -> String
hhMM = formatTime defaultTimeLocale "%H:%M"

fakeDay :: Day
fakeDay = fromGregorian 2020 1 1

instance Show LogEntry where
    show entry = hhMM (UTCTime fakeDay (sinceMid entry))
                 ++ " "
                 ++ (logEvent entry)

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

skipEOL :: Parser ()
skipEOL = skipMany (oneOf "\n")

skipRestOfLine :: Parser ()
skipRestOfLine = skipMany (noneOf "\n") >> skipEOL

skipComments :: Parser ()
skipComments = do
  _ <- count 2 $ char '-'
  skipRestOfLine

parseWeeHour :: Parser Int
parseWeeHour = do
  h <- char '0' <|> char '1'
  h' <- digit
  return $ (digitToInt h) * 10 + (digitToInt h')

parseLateHour :: Parser Int
parseLateHour = do
  h <- char '2'
  h' <- char '0' <|> char '1' <|> char '2' <|> char '3'
  return $ (digitToInt h) * 10 + (digitToInt h')

parseHour :: Parser Int
parseHour = (try parseWeeHour <|> parseLateHour) <* char ':'

parseMinute :: Parser Int
parseMinute = do
  m <- char '0' <|> char '1' <|> char '2' <|> char '3' <|> char '4' <|> char '5'
  m' <- digit
  return $ (digitToInt m) * 10 + (digitToInt m')

parseTimeStamp :: Parser DiffTime
parseTimeStamp = do
  h <- parseHour
  m <- parseMinute
  return $ realToFrac (h * 3600 + m * 60)

parseLogDay :: Parser Day
parseLogDay = do
  _ <- skipMany (oneOf "# ")
  yy <- integer <* char '-'
  mm <- integer <* char '-'
  dd <- integer
  skipRestOfLine
  let day = fromGregorianValid yy (fromIntegral mm) (fromIntegral dd)
  case day of
    (Just d) -> return d
    _ -> fail "Invalid date"
