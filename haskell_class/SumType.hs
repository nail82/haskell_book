data Airplane = T6 | R182 | PA28 deriving (Show, Eq)

instance Ord Airplane where
    compare R182 _    = GT
    compare _ R182    = LT
    compare PA28 T6   = GT
    compare T6 PA28   = LT
    compare _ _       = EQ



foldBool :: a -> a -> Bool -> a
foldBool x y z
    | z = x
    | otherwise = y

foldBool' :: a -> a -> Bool -> a
foldBool' x y z =
    case z of
      True  -> x
      False -> y

tensDigit :: Integral a => a -> a
tensDigit x = z
    where z = snd $ divMod x' 10
          x' = fst $ divMod x 10

hunDig :: Integral a => a -> a
hunDig x = z
  where z = snd $ divMod x' 100
        x' = fst $ divMod x 100
