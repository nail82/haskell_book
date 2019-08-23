module ListApplicative where

import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers


data List a =
    Nil
    | Cons a (List a)
    deriving (Eq, Show)

instance Semigroup (List a) where
    Nil <> xs  = xs
    xs  <> Nil = xs
    Cons x xs <> ys  = Cons x (xs <> ys)

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
    pure x = Cons x Nil
    Nil <*> _   = Nil
    _   <*> Nil = Nil
    Cons f fs <*> xs = (f <$> xs) <> (fs <*> xs)

instance (Arbitrary a) => Arbitrary (List a) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      elements [Cons a (Cons b Nil), Nil]

instance (Eq a) => EqProp (List a) where
    (=-=) = eq


{--
  This was my initial solution to (<*>)
    -- Cons f fs <*> Cons x xs = l1 <> l2 <> l3
    --     where l1 = Cons (f x) Nil
    --           l2 = Cons f Nil <*> xs
    --           l3 = fs <*> Cons x xs

  Note the similarity between the last function in the applicative
  instance and the last function in the Semigroup instance.  Both
  pull the head off the first list, do something with it and
  then combine the remainder of the first list with the second.

  You've got to pull apart the first list as you recurse or
  you'll get stuck in an infinite loop.

  Also note the combined usage of the angle bracket functions,
  <>, <$>, <*>.  I've see the pattern before and it looks
  to be handy.
--}


-- ZipList' example

take' :: Int -> List a -> List a
take' 0 _ = Nil
take' _ Nil = Nil
take' n (Cons x xs) = Cons x $ take' (n-1) xs

newtype ZipList' a =
    ZipList' (List a)
    deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
    xs =-= ys = xs' `eq` ys'
         where xs' = let (ZipList' l) = xs
                     in take' 3000 l
               ys' = let (ZipList' l) = ys
                     in take' 3000 l

instance Functor ZipList' where
    fmap f (ZipList' xs) =
        ZipList' $ fmap f xs

instance Applicative ZipList' where
    pure x = ZipList' $ pure x
    ZipList' fs <*> ZipList' xs =
        ZipList' $ fs `myApply` xs
            where myApply Nil _ = Nil
                  myApply _ Nil = Nil
                  myApply (Cons f fs') (Cons x xs') =
                      Cons (f x) $ fs' `myApply` xs'
