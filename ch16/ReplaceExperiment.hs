module ReplaceExperiment where

lms :: [Maybe [Char]]
lms = [Just "Ave", Nothing, Just "woohoo"]

replaceWithP :: b -> Char
replaceWithP = const 'p'

replaceWithP' :: [Maybe [Char]] -> Char
replaceWithP' = replaceWithP

liftedReplace :: Functor f => f a -> f Char
liftedReplace = fmap replaceWithP

liftedReplace' :: [Maybe [Char]] -> [Char]
liftedReplace' = liftedReplace

twiceLifted :: (Functor f1, Functor f) => f (f1 a) -> f (f1 Char)
twiceLifted = (fmap . fmap) replaceWithP

twiceLifted' :: [Maybe [Char]] -> [Maybe Char]
twiceLifted' = twiceLifted

thriceLifted :: (Functor f2, Functor f1, Functor f) => f (f1 (f2 a)) -> f (f1 (f2 Char))
thriceLifted = (fmap . fmap . fmap) replaceWithP

thriceLifted' :: [Maybe [Char]] -> [Maybe [Char]]
thriceLifted' = thriceLifted

replaceWith1 :: b -> Int
replaceWith1 = const 1

threeD :: [[[Integer]]]
threeD = [[[1,2,3], [4,5,6]], [[7,8,9], [10,11,12]]]

twoD :: [[Integer]]
twoD = [[1,2,3], [4,5,6]]

main :: IO ()
main = do
  print "expected: p"
  putStr "replaceWithP lms:  "
  print (replaceWithP lms)

  print "expected p"
  putStr "replaceWithP' lms:  "
  print (replaceWithP' lms)

  print "expected ppp"
  putStr "liftedReplace lms:  "
  print (liftedReplace lms)

  print "expected ppp"
  putStr "liftedReplace' lms:  "
  print (liftedReplace' lms)

  print "expected [Just p, Nothing, Just p]"
  putStr "twiceLifted lms:  "
  print (twiceLifted lms)

  print "expected [Just p, Nothing, Just p]"
  putStr "twiceLifted' lms:  "
  print (twiceLifted' lms)

  print "expected [Just ppp, Nothing, Just pppppp]"
  putStr "thriceLifted lms:  "
  print (thriceLifted lms)

  print "expected [Just ppp, Nothing, Just pppppp]"
  putStr "thriceLifted' lms:  "
  print (thriceLifted' lms)
