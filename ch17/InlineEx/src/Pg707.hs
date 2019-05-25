module Pg707 where

validateLength :: Int -> String -> Maybe String
validateLength n s
    | length s <= n = Just s
    | otherwise = Nothing

newtype Name =
    Name String deriving (Eq, Show)

newtype Address =
    Address String deriving (Eq, Show)

mkName :: String -> Maybe Name
mkName s = Name <$> validateLength 25 s

mkAddress :: String -> Maybe Address
mkAddress s = Address <$> validateLength 100 s

data Person =
    Person Name Address
    deriving (Eq, Show)

mkPerson :: String
            -> String
            -> Maybe Person
mkPerson name addr = Person <$> mkName name <*> mkAddress addr
