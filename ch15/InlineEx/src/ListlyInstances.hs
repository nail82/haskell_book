module ListlyInstances where

import Data.Monoid
import Listly

-- Don't do this.  Put instance declarations where the class is defined

instance Semigroup a => Semigroup (Listly a) where
    (<>) (Listly l) (Listly l') = Listly (l <> l')

instance Monoid a => Monoid (Listly a) where
    mempty = Listly []
    mappend = (<>)
