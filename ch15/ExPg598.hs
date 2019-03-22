module ExPg598 where

data Optional a =
                Nada
                | Only a
                deriving (Eq, Show)


instance Monoid a
    => Monoid (Optional a) where
        mempty  = Nada
        mappend = mappend
