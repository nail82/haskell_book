module MyWords where

myWords :: String -> [String]
myWords [] = []
myWords (' ':xs) = myWords xs
myWords xs = takeWhile (/= ' ') xs : myWords (dropWhile (/= ' ') xs)


myLines :: String -> [String]
myLines [] = []
myLines ('\n':xs) = myLines xs
myLines xs = takeWhile (/= '\n') xs : myLines (dropWhile (/= '\n') xs)

mypoem = "this is some text\nwith new lines included.\nwill it work?\n"
