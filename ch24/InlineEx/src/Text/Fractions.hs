{-# LANGUAGE OverloadedStrings #-}
module Text.Fractions where

import Control.Applicative
import Data.Ratio ((%))
import Text.Trifecta

badFraction = "1/0"
alsoBad = "10"
shouldWork = "1/2"
shouldAlsoWork = "2/1"

p :: Parser a -> String -> Result a
p f i = parseString f mempty i

parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  _ <- char '/'
  denominator <- decimal
  return (numerator % denominator)

parseReal :: Parser Rational
parseReal = do
  wholepart <- decimal
  _ <- char '.'
  trailing <- decimal
  let denom = 10 ^ (countDigits trailing)
  return $ (wholepart % 1) + (trailing % denom)

parseFractionOrReal :: Parser Rational
parseFractionOrReal = try parseFraction <|> try parseReal

countDigits :: Integer -> Integer
countDigits i = go i 0
    where go 0 n = n
          go i' n = go (i' `div` 10) (n+1)


virtuousFraction :: Parser Rational
virtuousFraction = do
  numerator <- decimal
  _ <- char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cannot be zero"
    _ -> return (numerator % denominator)
