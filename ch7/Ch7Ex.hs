module Ch7Ex where

decimalDig :: Integral a => a -> (a -> a)
decimalDig x y = mod (fst $ divMod y x) 10

tensDigit :: Integral a => a -> a
tensDigit = decimalDig 10

hunsD :: Integral a => a -> a
hunsD = decimalDig 100

foldBool :: a -> a -> Bool -> a
foldBool x y b
    | b == False = x
    | otherwise  = y


g :: (a -> b) -> (a, c) -> (b, c)
g aToB (a, c) = (aToB a, c)
