module Main where

import Text.Trifecta
import Data.Attoparsec.Text (parseOnly)

import Text.Fractions

main :: IO ()
main = do
  putStrLn "=> Attoparsec <="
  let attoP = parseOnly parseFraction
  print $ attoP badFraction
  print $ attoP alsoBad
  print $ attoP shouldWork
  print $ attoP shouldAlsoWork

  putStrLn "\n=> Trifecta <="
  let trifP i = parseString parseFraction mempty i
  print $ trifP badFraction
  print $ trifP alsoBad
  print $ trifP shouldWork
  print $ trifP shouldAlsoWork
