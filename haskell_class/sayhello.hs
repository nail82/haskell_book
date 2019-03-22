module Main where

sayHello :: String -> IO ()
sayHello s = putStrLn $ "Hi " ++ s ++ "!"


main :: IO ()
main = do
  name <- getLine
  sayHello name
