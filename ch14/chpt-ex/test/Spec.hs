module Main where

import Test.Hspec
import WordNumber

main :: IO ()
main = hspec $ do
         describe "digitToWord" $
                  do
                    it "returns zero for 0" $
                       do
                         digitToWord 0 `shouldBe` "zero"
                    it "returns one for 1" $
                       do
                         digitToWord 1 `shouldBe` "one"
                    it "returns toobig for 10" $
                       do
                         digitToWord 10 `shouldBe` "toobig"

         describe "digits" $
                  do
                    it "returns [1] for 1" $
                       do
                         digits 1 `shouldBe` [1]
                    it "returns [1,0,0] for 100" $
                       do
                         digits 100 `shouldBe` [1,0,0]

         describe "wordNumber" $
                  do
                    it "returns one for 1" $
                       do
                         wordNumber 1 `shouldBe` "one"
                    it "returns one-two for 12" $
                       do
                         wordNumber 12 `shouldBe` "one-two"
