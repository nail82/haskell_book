module BadMonad where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data CountMe a = CountMe Integer a
                 deriving (Eq, Show)

-- Broken fails fmap id == id
-- and fmap (f . g) = fmap f . fmap g
-- instance Functor CountMe where
--     fmap f (CountMe i a) =
--         CountMe (i + 1) (f a)

instance Functor CountMe where
    fmap f (CountMe i a) =
        CountMe i (f a)

instance Applicative CountMe where
    pure = CountMe 0
    CountMe n f <*> CountMe n' a =
        CountMe (n + n') (f a)

-- Fails left and right identity and associativity
-- instance Monad CountMe where
--     return = pure
--     CountMe n a >>= f =
--         let CountMe _ b = f a
--         in CountMe (n + 1) b

instance Monad CountMe where
    return = pure
    CountMe n a >>= f =
        let CountMe n' b = f a
        in CountMe (n + n') b

instance Arbitrary a
    => Arbitrary (CountMe a) where
        arbitrary =
            CountMe <$> arbitrary <*> arbitrary

instance Eq a => EqProp (CountMe a) where
    (=-=) = eq

doIt :: String -> CountMe String
doIt s = CountMe 1 (s ++ "bub")

main = do
  let trigger :: CountMe (Int, String, Int)
      trigger = undefined
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger
