module MyBind where

import Control.Monad (join)

bind :: Monad m => (a -> m b) -> m a -> m b
--bind :: (a -> m b) -> m a -> m b -- won't work
bind f ma = join (fmap f ma)



data MyData a b = Bub a | Joe b deriving (Eq, Show)

data MyMonoid = Sam  | Bob

instance Monoid MyMonoid where
    (<>) = undefined
