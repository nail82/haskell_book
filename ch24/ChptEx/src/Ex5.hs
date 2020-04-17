{-# LANGUAGE OverloadedStrings #-}
-- Log file parser
module Ex5 where

import Control.Applicative
import Text.Trifecta
import Data.Char (digitToInt)
import Data.CharSet as C
import Data.List (intercalate)
import Data.Time


-- --- Data ---
type Event = String

data LogEntry = LogEntry {
      sinceMid :: DiffTime,
      logEvent :: Event
    } deriving (Eq)

data LogDay = LogDay Day [LogEntry] deriving (Eq)

type Log = [LogDay]

hhMM :: FormatTime t => t -> String
hhMM = formatTime defaultTimeLocale "%H:%M"

fakeDay :: Day
fakeDay = fromGregorian 2020 1 1

instance Show LogEntry where
    show entry = hhMM (UTCTime fakeDay (sinceMid entry))
                 ++ " "
                 ++ (logEvent entry)

instance Show LogDay where
    show (LogDay d xs) =
        let fl = "# " ++ show d
            events = fl : (show <$> xs)
        in intercalate "\n" events

-- --- Functions ---
printables :: Parser Char
printables = oneOfSet $ C.fromList (['A'..'z'] ++ " 1234567890!@#$%^&*()+=,.?:;")

skipEOL :: Parser ()
skipEOL = skipMany (oneOf "\n")

skipRestOfLine :: Parser ()
skipRestOfLine = skipMany (noneOf "\n") >> skipEOL

skipComments :: Parser ()
skipComments = do
  _ <- count 2 $ char '-'
  skipRestOfLine

skipWhitespace :: Parser ()
skipWhitespace = skipMany (char ' ' <|> char '\t' <|> char '\n')

parseWeeHour :: Parser Int
parseWeeHour = do
  h <- char '0' <|> char '1'
  h' <- digit
  return $ sumPlaceValue h h'

parseLateHour :: Parser Int
parseLateHour = do
  h <- char '2'
  h' <- char '0' <|> char '1' <|> char '2' <|> char '3'
  return $ sumPlaceValue h h'

parseMinute :: Parser Int
parseMinute = do
  m <- char '0' <|> char '1' <|> char '2' <|> char '3' <|> char '4' <|> char '5'
  m' <- digit
  return $ sumPlaceValue m  m'

parseHour :: Parser Int
parseHour = (try parseWeeHour <|> parseLateHour) <* char ':'

sumPlaceValue :: Char -> Char -> Int
sumPlaceValue m l = (digitToInt m) * 10 + (digitToInt l)

parseTimeStamp :: Parser DiffTime
parseTimeStamp = do
  h <- parseHour
  m <- parseMinute
  return $ realToFrac (h * 3600 + m * 60)

-- This can leave some trailing whitespace
parseEvent :: Parser Event
parseEvent = do
  skipWhitespace
  ev <- some printables
  try skipComments <|> skipEOL
  return ev

parseDayStamp :: Parser Day
parseDayStamp = do
  skipWhitespace
  _ <- skipMany (oneOf "# ")
  yy <- integer <* char '-'
  mm <- integer <* char '-'
  dd <- integer
  try skipComments <|> skipEOL
  let day = fromGregorianValid yy (fromIntegral mm) (fromIntegral dd)
  case day of
    (Just d) -> return d
    _ -> fail "Invalid date"

parseLogEntry :: Parser LogEntry
parseLogEntry = skipWhitespace >> LogEntry <$> parseTimeStamp <*> parseEvent

parseLogDay :: Parser LogDay
parseLogDay = LogDay <$> parseDayStamp <*> (some parseLogEntry)

parseLog :: Parser Log
parseLog = skipComments >> skipWhitespace >> some parseLogDay
