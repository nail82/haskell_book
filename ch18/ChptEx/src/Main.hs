module Main where

import Control.Monad (join, liftM2)
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- Nope
data Nope a =
    NopeDotJpg
    deriving (Eq, Show)

instance Functor Nope where
    fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where
    pure _ = NopeDotJpg
    _ <*> NopeDotJpg = NopeDotJpg

instance Monad Nope where
    return = pure
    NopeDotJpg >>= _ = NopeDotJpg

instance Arbitrary a
    => Arbitrary (Nope a) where
        arbitrary = nopeGen

nopeGen :: Arbitrary a => Gen (Nope a)
nopeGen = do
  return NopeDotJpg

instance EqProp (Nope a) where (=-=) = eq

-- PhhbbttEither
data PhhbbttEither b a =
    PLeft a
  | PRight b
    deriving (Eq, Show)

instance Functor (PhhbbttEither b) where
    fmap _ (PRight b) = (PRight b)
    fmap f (PLeft  a) = PLeft (f a)

instance Applicative (PhhbbttEither b) where
    pure = PLeft
    PLeft f <*> PLeft a = PLeft (f a)
    PLeft _ <*> PRight b = PRight b
    PRight a <*> _ = PRight a

instance Monad (PhhbbttEither b) where
    return = pure
    PLeft x >>= f = f x
    PRight y >>= _ = PRight y

instance (Arbitrary b, Arbitrary a) => Arbitrary (PhhbbttEither b a)
    where arbitrary = phhbbttEitherGen

phhbbttEitherGen :: (Arbitrary b, Arbitrary a) => Gen (PhhbbttEither b a)
phhbbttEitherGen = do
  b <- arbitrary
  a <- arbitrary
  elements [PLeft a, PRight b]

instance (EqProp b, EqProp a) => EqProp (PhhbbttEither b a) where
    (PLeft b)  =-= (PLeft b')  = b =-= b'
    (PRight a) =-= (PRight a') = a =-= a'
    _          =-=           _ = property False

-- Identity

newtype Identity a = Identity a
    deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
    pure = Identity
    Identity f <*> Identity a = Identity (f a)

instance Monad Identity where
    return = pure
    Identity a >>= f = f a

instance (Arbitrary a) => Arbitrary (Identity a) where
    arbitrary = genIdentity

genIdentity :: (Arbitrary a) => Gen (Identity a)
genIdentity = do
  a <- arbitrary
  return (Identity a)

instance (EqProp a) => EqProp (Identity a) where
    (Identity a) =-= (Identity a') = a =-= a'

-- List
data List a =
    Nil
  | Cons a (List a)
    deriving (Eq, Show)

instance Semigroup (List a) where
    Nil <> xs  = xs
    xs  <> Nil = xs
    Cons x xs <> ys  = Cons x (xs <> ys)

instance Functor List where
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)
    fmap _ Nil         = Nil

instance Applicative List where
    pure  x = Cons x Nil
    Nil <*> _   = Nil
    _   <*> Nil = Nil
    Cons f fs <*> xs = (f <$> xs) <> (fs <*> xs)

{--

I've seen at least one solution to the Monad List exercise that
defines `join :: List a' as `(=<<) id'.  That solution doesn't work
because the compiler will define (=<<) in terms of (>>=) and the
interlocking (and incomplete) definition will drop into an infinite
loop.

`(=<<) id' is a valid expression of join, but the associated Monad
must be fully defined for it to operate correctly.

--}
instance Monad List where
    return = pure
    xs  >>= f = lj (f <$> xs)
        where lj Nil = Nil
              lj (Cons xs' Nil) = xs'
              lj (Cons x (Cons y ys)) = x <> y  <> (lj ys)

{--
Cons (Cons 1 Nil) (Cons Nil Nil)
(:)  ((:)  1 [])  ((:)  []  [])

Cons (Cons 1 Nil) (Cons (Cons 2 Nil) Nil)
(:)  ((:)  1 [] ) ((:)  ((:)  2 [] ) [] )

Cons Nil (Cons (Cons 2 Nil) Nil)
(:)  []  ((:)  ((:)  2 [])  [] )

Cons (Cons 1 Nil) (Cons (Cons 2 Nil) Nil)
(:)  ((:)  1 [] ) ((:)  ((:)  2 [] ) [] )
--}


-- | 789.1
j :: Monad m => m (m a) -> m a
j = (=<<) id

-- | 789.2
l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

-- | 789.3
{--
This is liftA2.  Something like this might work:

l2' :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
l2' f (Just x) mb = fmap (f x) mb

However, we don't know anything about the Monad class
we're handling.
--}
l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f ma mb = do
  a' <- ma
  b' <- mb
  return $ f a' b'

-- | 789.4
ap :: Monad m => m a -> m (a -> b) -> m b
ap = flip (<*>)

-- | 789.5


-- Had to get some help with this one from:
-- github.com/ArulselvanMadhavan/haskell-first-principles/blob/master/chapter18/src/Ex2.hs
meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (a:as) f = do
  b <- f a
  bs <- meh as f
  return $ b:bs
--Written with bind
--meh (a:as) f = (f a) >>= (\b -> (meh as f) >>= (\bs -> return $ b:bs))

meh2 :: Monad m => a -> (a -> m b) -> m [b]
meh2 a f = do
  b <- (f a)
  return [b]


-- | 789.6
flipType :: Monad m => [m a] -> m [a]
flipType mas = meh mas id



main :: IO ()
main = do
  putStrLn("==> Nope <==")
  let trigger :: Nope (Int, String, Int)
      trigger = undefined
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger

  putStrLn("==> PhhbbttEither <==")
  let trigger1 :: PhhbbttEither (Int, String, Int) (Int, String, Int)
      trigger1 = undefined
  quickBatch $ functor trigger1
  quickBatch $ applicative trigger1
  quickBatch $ monad trigger1

  putStrLn("==> Identity <==")
  let trigger2 :: Identity (Int, String, Int)
      trigger2 = undefined
  quickBatch $ functor trigger2
  quickBatch $ applicative trigger2
  quickBatch $ monad trigger2
