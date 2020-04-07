{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import qualified Data.Attoparsec.ByteString as A
import Data.Attoparsec.ByteString (parseOnly)
import Data.ByteString (ByteString)
import Text.Trifecta hiding (parseTest)
import Text.Parsec (Parsec, parseTest)

trifP :: Show a => Parser a -> String -> IO ()
trifP p s = print $ parseString p mempty s

parsecP :: Show a => Parsec String () a -> String -> IO ()
parsecP = parseTest

attoP :: Show a => A.Parser a -> ByteString -> IO ()
attoP p a = print $ parseOnly p a

noBackParse :: (Monad f, CharParsing f) => f Char
noBackParse = (char '1' >> char '2') <|> char '3'

tryParse :: (Monad f, CharParsing f) => f Char
tryParse = try (char '1' >> char '2') <|> char '3'

tryAnnot :: (Monad f, CharParsing f) => f Char
tryAnnot = (try (char '1' >> char '2') <?> "Tried 12")
           <|> (char '3' <?> "Tried 3")

main :: IO ()
main = do
  putStrLn "\n=> trifecta <="
  trifP noBackParse "13"
  trifP tryParse "13"
  trifP tryAnnot "13"

  putStrLn "\n=> parsec <="
  parsecP noBackParse "13"
  parsecP tryParse "13"
  parsecP tryAnnot "13"

  putStrLn "\n=> atto <="
  attoP noBackParse "13"
  attoP tryParse "13"
  attoP tryAnnot "13"
