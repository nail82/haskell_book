{-# LANGUAGE OverloadedStrings #-}
module Text.Fractions where

import Control.Applicative
import Data.Ratio ((%))
import Data.String (IsString)
import Text.Trifecta

badFraction :: IsString s => s
badFraction = "1/0"

alsoBad :: IsString s => s
alsoBad = "10"

shouldWork :: IsString s => s
shouldWork = "1/2"

shouldAlsoWork :: IsString s => s
shouldAlsoWork = "2/1"

parseFraction :: (Monad m, TokenParsing m) => m Rational
parseFraction = do
  num <- integer
  _ <- char '/'
  denom <- integer
  case denom of
    0 -> fail "Denominator can't be zero"
    _ -> return (num % denom)
