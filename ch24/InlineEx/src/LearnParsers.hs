module LearnParsers where

import Text.Trifecta

stop :: Parser a
stop = unexpected "stop"

one :: Parser Char
one = char '1'

two :: Parser Char
two = char '2'

three :: Parser Char
three = char '3'


--oneTwo :: Parser ()
--oneTwo = char '1' >> char '2' >> eof
oneTwo :: Parser Char
oneTwo = char '1' >> char '2'

oneTwo' :: Parser Char
oneTwo' = oneTwo >> stop

pNL :: String -> IO ()
pNL s =
    putStrLn ('\n' : s)

testParse :: Parser Char -> IO ()
testParse p =
    print $ parseString p mempty "123"

testParseSt :: String -> Parser Char -> IO ()
testParseSt s p =
    print $ parseString p mempty s


main :: IO ()
main = do
  pNL "stop:"
  testParse stop
  pNL "one:"
  testParse one
  pNL "oneTwo:"
  testParse oneTwo
  pNL "oneTwo':"
  testParse oneTwo'
