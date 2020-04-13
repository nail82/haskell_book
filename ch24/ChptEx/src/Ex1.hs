{-# LANGUAGE OverloadedStrings #-}
module Ex1 where
-- Semantic versioning
{-
<version core>
<version core> - <pre-release>
<version core> + <build>
<version core> - <pre-release> + <build>
-}

import Control.Applicative
import Text.Trifecta

data NumberOrString = NOSS String
                    | NOSI Integer
                      deriving (Eq, Show)

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer = SemVer Major Minor Patch Release Metadata
            deriving (Eq, Show)

parseNumOrString :: Parser NumberOrString
parseNumOrString = (NOSS <$> some letter) <|> (NOSI <$> integer)

dotSeparated :: Parser [NumberOrString]
dotSeparated = sepBy parseNumOrString dot

releaseParser :: Parser Release
releaseParser = dash >> dotSeparated

buildParser :: Parser Metadata
buildParser = plus >> dotSeparated

plus :: Parser Char
plus = char '+'

dash :: Parser Char
dash = char '-'

semVerParser :: Parser SemVer
semVerParser = do
  mj <- integer <* dot
  mi <- integer <* dot
  pt <- integer
  maybe_release <- optional releaseParser
  maybe_build <- optional buildParser
  let rel = case maybe_release of
              Just xs -> xs
              _ -> []
  let bld = case maybe_build of
              Just xs -> xs
              _ -> []
  return (SemVer mj mi pt rel bld)
