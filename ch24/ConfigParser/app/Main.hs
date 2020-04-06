{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Map as M
import Test.Hspec
import Text.Trifecta
import Data.Ini

main :: IO ()
main = hspec $ do
         describe "Assignment Parsing" $
               it "can parse a simple assignment" $ do
                 let m = parseByteString parseAssignment mempty assignmentEx
                     r' = maybeSuccess m
                 print m
                 r' `shouldBe` Just ("woot", "1")
         describe "Header Parsing" $
               it "can parse a header" $ do
                 let m = parseByteString parseHeader mempty headerEx
                     r' = maybeSuccess m
                 print m
                 r' `shouldBe` Just (Header "blah")
         describe "Section Parsing" $
               it "can parse a section" $ do
                 let m = parseByteString parseSection mempty sectionEx
                     r' = maybeSuccess m
                 print m
                 r' `shouldBe` Just
                        (Section (Header "states")
                         (M.fromList [("Chris", "Texas")]))

-- A few more tests defined in the text, but moving on.
