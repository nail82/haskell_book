{-# LANGUAGE OverloadedStrings #-}
module Ex4 where
-- parse phone numbers
{-
123-456-7890
1234567890
(123) 456-7890
1-123-456-7890
-}
import Ex2 (expandDigitList)

import Control.Applicative
import Text.Trifecta


type NumberingPlanArea = Int
type Exchange = Int
type LineNumber = Int

data PhoneNumber = PhoneNumber NumberingPlanArea Exchange LineNumber
                   deriving (Eq, Show)

dash :: Parser Char
dash = char '-'

parseNDigits :: Int -> Parser Int
parseNDigits n = do
  xs <- count n digit
  return $ fromIntegral $ expandDigitList xs

parseThree :: Parser Int
parseThree = parseNDigits 3

parseFour :: Parser Int
parseFour = parseNDigits 4

parseAreaCode :: Parser Int
parseAreaCode = (try (parseThree <* dash))
                <|> (try (char '(' *> parseThree <* char ')'))
                <|> (try (char '1' >> dash >> parseThree <* dash))
                <|> (try parseThree)

parseExchange :: Parser Int
parseExchange = (try (parseThree <* dash))
                <|> (try (someSpace >> parseThree <* dash))
                <|> (try parseThree)

parseLineNumber :: Parser Int
parseLineNumber = parseFour

parsePhone :: Parser PhoneNumber
parsePhone = PhoneNumber
             <$> parseAreaCode
             <*> parseExchange
             <*> parseLineNumber
