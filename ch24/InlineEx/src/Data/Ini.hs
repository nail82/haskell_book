{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Data.Ini where

import Control.Applicative
import Data.ByteString (ByteString)
import Data.Char (isAlpha)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import Data.Text.IO as TIO
import Test.Hspec
import Text.RawString.QQ
import Text.Trifecta

headerEx :: ByteString
headerEx = "[blah]"

assignmentEx :: ByteString
assignmentEx = "woot=1"

commentEx :: ByteString
commentEx = "; blah\n; woot\n ;joe"

sectionEx :: ByteString
sectionEx =
    "; ignore\n[states]\nChris=Texas"

sectionEx' :: ByteString
sectionEx' = [r|
; ignore
[states]
Chris=Texas
|]

sectionEx'' :: ByteString
sectionEx'' = [r|
; comment
[section]
host=wikipedia.org
alias=claw

[whatisit]
red = intoothandclaw
|]


type Name = String
type Value = String
type Assignments = Map Name Value

newtype Header =
    Header String
    deriving (Eq, Ord, Show)

data Section =
    Section Header Assignments
    deriving (Eq, Show)

newtype Config =
    Config (Map Header Assignments)
    deriving (Eq, Show)

parseBracketPair :: Parser a -> Parser a
parseBracketPair p =
    char '[' *> p <* char ']'

parseHeader :: Parser Header
parseHeader = parseBracketPair (Header <$> some letter)

parseAssignment :: Parser (Name, Value)
parseAssignment = do
  name <- some letter
  skipWhitespace
  _ <- char '='
  skipWhitespace
  val <- some (noneOf "\n")
  skipEOL
  return (name, val)

skipEOL :: Parser ()
skipEOL = skipMany (oneOf "\n")

skipComments :: Parser ()
skipComments =
    skipMany (do _ <- char ';' <|> char '#'
                 skipMany (noneOf "\n")
                 skipEOL)

skipWhitespace :: Parser ()
skipWhitespace = skipMany (char ' ' <|> char '\n' <|> char '\t')

parseSection :: Parser Section
parseSection = do
  skipWhitespace
  skipComments
  h <- parseHeader
  skipEOL
  assignments <- some parseAssignment
  return $ Section h (M.fromList assignments)

rollup :: Section
       -> Map Header Assignments
       -> Map Header Assignments
rollup (Section h a) m = M.insert h a m

parseIni :: Parser Config
parseIni = do
  sections <- some parseSection
  return $ Config (foldr rollup M.empty sections)

maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing
