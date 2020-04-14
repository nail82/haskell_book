{-# LANGUAGE OverloadedStrings #-}
module Ex3 where
-- Extend exercise 2 to include negative integers
import Ex2
import Text.Trifecta

base10Integer' :: Parser Integer
base10Integer' = do
  sign <- optional $ char '-'
  num <- base10Integer
  return $ case sign of
             Just '-' -> (-1) * num
             _ -> num
