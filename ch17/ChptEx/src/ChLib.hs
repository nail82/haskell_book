module ChLib where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Control.Applicative

someFunc :: IO ()
someFunc = putStrLn "someFunc"


-- 741.1 through 741.4 are about getting the type signature correct
-- pg 741.1 []
pureList :: a -> [a]
pureList x = pure x

listTieFtr :: [(a -> b)] -> [a] -> [b]
listTieFtr fs xs = fs <*> xs

-- pg 741.2 IO
pureIO :: a -> IO a
pureIO x = pure x

ioTieFtr :: IO (a -> b) -> IO a -> IO b
ioTieFtr ioF ioA = ioF <*> ioA

-- pg 741.3 (,) a
-- For some intuition
pureStringTup :: a -> (String, a)
pureStringTup x = pure x

stringTupTieFtr :: (String, a -> b) -> (String, a) -> (String, b)
stringTupTieFtr t1 t2 = t1 <*> t2

pureTup :: Monoid a => a -> ((,) a) a
pureTup x = pure x

tupTieFtr :: Monoid a => ((,) a) (a -> b) -> ((,) a) a -> ((,) a) b
tupTieFtr f t = f <*> t


-- pg 741.4 (->) e
{--
  When pure applied to a value, it a function that returns the value of the argument to pure.
  i.e.  pureFa 2 is a constant function that always returns 2

  When applied to a function, pure returns a function with an additional, ignored parameter.
  i.e. let f = \x y -> x + 1
  let f' = pureFa f
  f  :: a -> a -> a
  f' :: e -> a -> a -> a (the parameter `e' is ignored)
--}
pureFa :: a -> ((->) e) a
pureFa x = pure x

funAppTieFtr :: ((->) e) (a -> b) -> ((->) e) a -> ((->) e) b
funAppTieFtr f t = f <*> t

-- 741 Pt 2
-- 1.

data Pair a = Pair a a deriving Show

instance Functor Pair where
    fmap f (Pair a a') = Pair (f a) (f a')

instance Applicative Pair where
    pure x  = Pair x x
    Pair f f' <*> Pair a a' = Pair (f a) (f' a')

instance (Eq a) => EqProp (Pair a) where
    (Pair a b) =-= (Pair a' b') = property ((a == a') && (b == b'))

instance Arbitrary a
    => Arbitrary (Pair a) where
        arbitrary = pairGen

pairGen :: Arbitrary a => Gen (Pair a)
pairGen = do
  a  <- arbitrary
  a' <- arbitrary
  return (Pair a a')

-- 742
-- 2.
data Two a b = Two a b deriving Show

instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

instance (Monoid a) => Applicative (Two a) where
    pure x = Two mempty x
    Two a f <*> Two a' b = Two (a <> a') (f b)

instance (Monoid a, Arbitrary a, Arbitrary b)
    => Arbitrary (Two a b) where
        arbitrary = twoGen

instance (Eq a, Eq b) => EqProp (Two a b) where
    Two a b =-= Two a' b' = property ((a == a') && (b == b'))

twoGen :: (Monoid a, Arbitrary a, Arbitrary b) => Gen (Two a b)
twoGen = do
  a <- arbitrary
  b <- arbitrary
  return (Two a b)


-- 742
-- 3.
data Three a b c = Three a b c deriving Show

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
    pure x = Three mempty mempty x
    Three a b f <*> Three a' b' c = Three (a <> a') (b <> b') (f c)

instance (Monoid a, Monoid b, Arbitrary a, Arbitrary b, Arbitrary c)
    => Arbitrary (Three a b c) where
        arbitrary = threeGen

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
    Three a b c =-= Three a' b' c' = property (a == a'
                                              && b == b'
                                              && c == c'
                                              )

threeGen :: (Monoid a
            , Monoid b
            , Arbitrary a
            , Arbitrary b
            , Arbitrary c) => Gen (Three a b c)
threeGen = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (Three a b c)

-- 742
-- 4.
data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
    fmap f (Three' a b b') = Three' a (f b) (f b')

instance (Monoid a) => Applicative (Three' a) where
    pure x = Three' mempty x x
    Three' a f g <*> Three' a' b b' = Three' (a <> a') (f b) (g b')

instance (Monoid a, Arbitrary a, Arbitrary b)
    => Arbitrary (Three' a b) where
        arbitrary = threePGen

instance (Eq a, Eq b) => EqProp (Three' a b) where
    Three' a b c =-= Three' a' b' c' = property (a == a'
                                                && b == b'
                                                && c == c'
                                                )

threePGen :: (Monoid a, Arbitrary a, Arbitrary b) => Gen (Three' a b)
threePGen = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (Three' a b c)

-- 742
-- 5.
data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Monoid a, Monoid b, Monoid c) => Functor (Four a b c) where
    fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a
         , Monoid b
         , Monoid c
         ) => Applicative (Four a b c) where
    pure x = Four mempty mempty mempty x
    Four a b c f <*> Four a' b' c' d = Four (a <> a') (b <> b') (c <> c') (f d)

instance (Monoid a
         , Monoid b
         , Monoid c
         , Arbitrary a
         , Arbitrary b
         , Arbitrary c
         , Arbitrary d ) => Arbitrary (Four a b c d) where
    arbitrary = fourGen

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
    Four a b c d =-= Four a' b' c' d' = property (a == a'
                                                 && b == b'
                                                 && c == c'
                                                 && d == d')

fourGen :: (Monoid a
           , Monoid b
           , Monoid c
           , Arbitrary a
           , Arbitrary b
           , Arbitrary c
           , Arbitrary d) => Gen (Four a b c d)
fourGen = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  d <- arbitrary
  return $ Four a b c d


-- 742
-- 6.
data Four' a b = Four' a a a b deriving (Eq, Show)

instance (Monoid a) => Functor (Four' a) where
    fmap f (Four' a a' a'' b) = Four' a a' a'' (f b)

instance (Monoid a) => Applicative (Four' a) where
    pure x = Four' mempty mempty mempty x
    Four' a a' a'' f <*> Four' a1 a2 a3 b = Four' (a <> a1) (a' <> a2) (a'' <> a3) (f b)

instance (Monoid a, Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
    arbitrary = fourPGen

instance (Eq a, Eq b) => EqProp (Four' a b) where
    Four' a a' a'' b =-= Four' a1 a1' a1'' b' = property (a     == a1
                                                         && a'  == a1'
                                                         && a'' == a1''
                                                         && b   == b')


fourPGen :: (Monoid a, Arbitrary a, Arbitrary b) => Gen (Four' a b)
fourPGen = do
  a   <- arbitrary
  a'  <- arbitrary
  a'' <- arbitrary
  b   <- arbitrary
  return $ Four' a a' a'' b
