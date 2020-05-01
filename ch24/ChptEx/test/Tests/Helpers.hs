module Tests.Helpers (maybeSuccess, pb) where

import Text.Trifecta

maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing

pb :: Parser a -> String -> Result a
pb f s = parseString f mempty s
