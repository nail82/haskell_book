module Lib where

import Control.Monad

-- "In terms of fmap and join"
-- bind is also (=<<)
bind :: Monad m => (a -> m b) -> m a -> m b
bind f ma = join $ fmap f ma

-- :t fmap
-- fmap :: Functor f => (a -> b) -> f a -> f b

-- :t join
-- join :: Monad m => m (m a) -> m a

sequencing :: IO ()
sequencing = do
  putStrLn "blah"
  putStrLn "another thing"

sequencing' :: IO ()
sequencing' = do
  putStrLn "blah" >> putStrLn "another thing"

binding :: IO ()
binding = do
  name <- getLine
  putStrLn name

binding' :: IO ()
binding' = do
  getLine >>= putStrLn

data Cow = Cow {
      name :: String
    , age  :: Int
    , weight :: Int
    } deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty n  = Just n

noNegative :: Int -> Maybe Int
noNegative x
    | x < 0 = Nothing
    | otherwise = Just x

weightCheck :: Cow -> Maybe Cow
weightCheck c =
    let w = weight c
        n = name c
    in if n == "Bess" && w > 499
       then Nothing
       else Just c

mkSphericalCow :: String ->
                  Int ->
                  Int ->
                  Maybe Cow
mkSphericalCow n a w = do
    n' <- noEmpty n
    a' <- noNegative a
    w' <- noNegative w
    weightCheck (Cow n' a' w')

mkSphericalCow' :: String ->
                   Int ->
                   Int ->
                   Maybe Cow
mkSphericalCow' n a w =
  noEmpty n >>=
              \nammy ->
                noNegative a >>=
                \agey ->
                  noNegative w >>=
                  \weighty ->
                    weightCheck (Cow nammy agey weighty)



f :: Int -> Maybe Int
f x = Just (x + 2)

g :: Int -> Maybe Int
g x = Just (x * 3)
