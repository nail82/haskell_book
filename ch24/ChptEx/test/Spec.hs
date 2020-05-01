module Main where

import Tests.MyHspecs
import Tests.MyQC

main :: IO ()
main = hspecTests >> runQC
