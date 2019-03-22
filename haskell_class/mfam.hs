-- Monoid/Functor/Applicative/Monad
import Data.Monoid
import Control.Monad

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Monoid a => Monoid (Identity a) where
    mempty = (Identity mempty)
    mappend (Identity a) (Identity a') = Identity (a <> a')

data Or b a = Faust a
            | Slytherin b deriving Show

instance Functor (Or b) where
    fmap _ (Slytherin b) = Slytherin b
    fmap f (Faust a) = Faust (f a)


newtype Constant a b =
    Constant { getConstant :: a}
    deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

-- Identity
instance Applicative Identity where
    pure a = Identity a
    (<*>) (Identity f) (Identity google) = Identity (f google)

instance Monoid a => Applicative (Constant a) where
    pure _ = Constant mempty
    (<*>)  (Constant f) (Constant a) = Constant (f `mappend` a)

instance Monad Identity where
    return = pure
    (>>=) (Identity a) f = f a
