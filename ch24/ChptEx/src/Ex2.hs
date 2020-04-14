{-# LANGUAGE OverloadedStrings #-}
module Ex2 where

import Text.Trifecta
import Control.Applicative
import Data.Char (digitToInt)

-- Positive integer values

parseDigit :: Parser Char
parseDigit = char '0'
             <|> char '1'
             <|> char '2'
             <|> char '3'
             <|> char '4'
             <|> char '5'
             <|> char '6'
             <|> char '7'
             <|> char '8'
             <|> char '9'

base10Integer :: Parser Integer
base10Integer = do
  xs <- some parseDigit
  return $ expandDigitList xs

pow10 :: (Char, Int) -> Integer -> Integer
pow10 (c, i) z = fromIntegral (digitToInt c) * 10^i + z

expandDigitList :: [Char] -> Integer
expandDigitList xs = foldr pow10 0 $ zip (reverse xs) [0..maxp]
                     where maxp = length xs - 1
