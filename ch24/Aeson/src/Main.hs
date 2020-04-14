{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
-- Marshalling module in the book
module Main where

import Control.Applicative
import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Text (Text)
import Text.RawString.QQ
import Data.Scientific (floatingOrInteger)

sectionJson :: LBS.ByteString
sectionJson = [r|
{ "section":  {"host": "wikipedia.org" },
  "whatisit": {"red" : "intoothandclaw"}
}
|]

data TestData =
    TestData {
      section :: Host
    , what :: Color
    } deriving (Eq, Show)

newtype Host =
    Host String
    deriving (Eq, Show)

type Annotation = String

data Color = Red Annotation
           | Blue Annotation
           | Yellow Annotation
             deriving (Eq, Show)

instance FromJSON TestData where
    parseJSON (Object v) =
        TestData <$> v .: "section"
                 <*> v .: "whatisit"
    parseJSON _ =
        fail "Expected a TestData object"

instance FromJSON Host where
    parseJSON (Object v) =
        Host <$> v .: "host"
    parseJSON _ = fail "Expected a host object"

instance FromJSON Color where
    parseJSON (Object v) = (Red <$> v .: "red")
                           <|> (Blue <$> v .: "blue")
                           <|> (Yellow <$> v .: "yellow")
    parseJSON _ = fail "Expected a color object"

main :: IO ()
main = do
  putStrLn "\n"
  let blah :: Maybe TestData
      blah = decode sectionJson
  case blah of
    (Just j) -> print j
    _ -> putStrLn "Decode failed"
  putStrLn "\n"
