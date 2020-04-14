{-# LANGUAGE OverloadedStrings #-}
module Ex4 where
-- parse phone numbers
{-
123-456-7890
123456789
(123) 456-7890
1-123-456-7890
-}
import Ex2

import Control.Applicative
import Text.Trifecta


type NumberingPlanArea = Int
type Exchange = Int
type LineNumber = Int

data PhoneNumber = PhoneNumber NumberingPlanArea Exchange LineNumber
                   deriving (Eq, Show)

dash :: Parser Char
dash = char '-'

skipWhitespace :: Parser ()
skipWhitespace = skipMany (char ' ' <|> char '\n' <|> char '\t')

parseNDigits :: Int -> Parser Int
parseNDigits n = do
  xs <- count n digit
  return $ fromIntegral $ expandDigitList xs

parseThree :: Parser Int
parseThree = parseNDigits 3

parseFour :: Parser Int
parseFour = parseNDigits 4

parseDelimited :: Parser PhoneNumber
parseDelimited = do
  ac <- parseThree <* dash
  ex <- parseThree <* dash
  ln <- parseFour
  return $ PhoneNumber ac ex ln

parseNoDelimited :: Parser PhoneNumber
parseNoDelimited = do
  ac <- parseThree
  ex <- parseThree
  ln <- parseFour
  return $ PhoneNumber ac ex ln

parseBracketedAc :: Parser PhoneNumber
parseBracketedAc = do
  ac <- char '(' *> parseThree <* char ')'
  ex <- skipWhitespace >> parseThree <* dash
  ln <- parseFour
  return $ PhoneNumber ac ex ln

parseCountryCode :: Parser PhoneNumber
parseCountryCode = do
  ac <- char '1' >> dash >> parseThree <* dash
  ex <- parseThree <* dash
  ln <- parseFour
  return $ PhoneNumber ac ex ln

parsePhone :: Parser PhoneNumber
parsePhone = undefined
