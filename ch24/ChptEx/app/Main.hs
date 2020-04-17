{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Main where

import Data.ByteString as T
import System.Environment (getArgs)
import System.Exit ( exitFailure
                   , exitSuccess)
import Text.Trifecta
import qualified Data.List as L

import Ex1
import Ex2
import Ex4
import Ex5
import Ex5Data

p f s = parseString f mempty s

main :: IO ()
main = do
  -- Ex 1
  print $ p semVerParser justCore
  print $ p semVerParser coreAndRelease
  print $ p semVerParser coreAndBuild
  print $ p semVerParser coreReleaseBuild
  -- Ex 2
  print $ p base10Integer tenDigits
  -- Ex 4
  print $ p parsePhone dashDelimited
  print $ p parsePhone noDashes
  print $ p parsePhone oldWay
  print $ p parsePhone countryCode
  print $ p parsePhone countrySpaceDelimited
  args <- getArgs
  case args of
    [] -> argError
    fnm:_ -> parseLogFile fnm
  where argError = do
          Prelude.putStrLn "Need a filename"
          exitFailure
        parseLogFile fnm = do
                     Prelude.putStrLn $ "Parsing " ++ fnm
                     logData <- T.readFile fnm
                     let res = parseByteString parseLog mempty logData
                     case res of
                       (Success logs) -> Prelude.putStrLn $ L.intercalate "\n" $ show <$> logs
                       (Failure perr) -> Prelude.putStrLn $ show perr
                     exitSuccess
