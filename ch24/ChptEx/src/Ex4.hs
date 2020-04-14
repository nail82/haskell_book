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
parseDelimited = PhoneNumber
                 <$> parseThree <* dash
                 <*> parseThree <* dash
                 <*> parseFour

parseNoDelimited :: Parser PhoneNumber
parseNoDelimited = PhoneNumber
                   <$> parseThree
                   <*> parseThree
                   <*> parseFour

parseBracketedAc :: Parser PhoneNumber
parseBracketedAc = PhoneNumber
                   <$> (char '(' *> parseThree <* char ')')
                   <*> (skipWhitespace >> parseThree <* dash)
                   <*> parseFour

parseCountryCode :: Parser PhoneNumber
parseCountryCode = PhoneNumber
                   <$> (char '1' >> dash >> parseThree <* dash)
                   <*> (parseThree <* dash)
                   <*> parseFour

parsePhone :: Parser PhoneNumber
parsePhone = undefined
