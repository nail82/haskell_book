module LearnParsers where

import Text.Trifecta
import Control.Applicative

stop :: (Show a) => Parser a
stop = unexpected "stop"

one :: Parser Char
one = char '1'

two :: Parser Char
two = char '2'

three :: Parser Char
three = char '3'

oneTwo :: Parser Char
oneTwo = char '1' >> char '2'

oneTwo' :: Parser Char
oneTwo' = oneTwo >> stop

oneTwoEof :: Parser ()
oneTwoEof = char '1' >> char '2' >> eof

oneAnda :: Parser ()
oneAnda = (string "123" >> eof)
          <|> (string "12" >> eof)
          <|> (string "1" >> eof)

oneAnda' :: Parser String
oneAnda' = (string "123"    <* eof)
           <|> (string "12" <* eof)
           <|> (string "1"  <* eof)


pNL :: String -> IO ()
pNL s = putStrLn ('\n' : s)

testParse :: (Show a) => Parser a -> IO ()
testParse p =
    print $ parseString p mempty "123"

testParseSt :: (Show a) => String -> Parser a -> IO ()
testParseSt s p =
    print $ parseString p mempty s
