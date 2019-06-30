module MonadComposition where

import Control.Monad

mcomp :: Monad m =>
         (b -> m c)
         -> (a -> m b)
         -> a -> m c
mcomp f g a = join $ fmap f (g a)

outerF :: Int -> Maybe Int
outerF x = Just (x + 1)

innerF :: Int -> Maybe Int
innerF y = Just (y * 2)

innerListF :: Int -> [Int]
innerListF x = take x $ repeat x

outerListF :: Int -> [Int]
outerListF y = [y * 2]
