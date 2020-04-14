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
  let max_pwr = (length xs) - 1
      tups = zip (reverse xs) [0..max_pwr]
  return $ foldr pow10 0 tups

pow10 :: (Char, Int) -> Integer -> Integer
pow10 (c, i) z = fromIntegral (digitToInt c) * 10^i + z
