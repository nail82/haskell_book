module LearnParsers where

import Text.Trifecta

stop :: Parser Char
stop = unexpected "stop"

one :: Parser Char
one = char '1'

oneEof :: (Monad m, CharParsing m) => m ()
oneEof = char '1' >> eof

oneTwoEof :: (Monad m, CharParsing m) => m ()
oneTwoEof = char '1' >> char '2' >> eof

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

testParse :: (Show a) => Parser a -> IO ()
testParse p =
    print $ parseString p mempty "123"

testParseSt :: String -> Parser Char -> IO ()
testParseSt s p =
    print $ parseString p mempty s
