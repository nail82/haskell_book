module SayHello where

sayHello :: String -> IO()
sayHello x =
    putStrLn("Hello, " ++ x ++ "!")

triple x = x * 3

square x = x * x

piRsquare r = pi * square r
