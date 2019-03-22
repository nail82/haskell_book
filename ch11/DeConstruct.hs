module DeConstruct where

--data Name = Name String deriving Show
newtype Name = Name String deriving Show
newtype Acres = Acres Int deriving Show

data FarmerType = DairyFarmer
                | WheatFarmer
                | SoybeanFarmer
                deriving Show

data Farmer =
            Farmer Name Acres FarmerType
            deriving Show

isDairyFarmer :: Farmer -> Bool
isDairyFarmer (Farmer _ _ DairyFarmer) = True
isDairyFarmer _ = False


data FarmerRec =
               FarmerRec { name :: String
                         , acres :: Int
                         , farmerType :: FarmerType }
               deriving Show

isDairyFarmer' :: FarmerRec -> Bool
isDairyFarmer' farmer =
    case farmerType farmer of
      DairyFarmer -> True
      _           -> False

isDairyFarmer'' :: FarmerRec -> Bool
isDairyFarmer'' (FarmerRec _ _ DairyFarmer) = True
isDairyFarmer'' _ = False

type DocVersion = Int

data EsResultFound a =
                     EsResultFound { _version :: DocVersion
                                   , _source :: a
                                   } deriving (Eq, Show)
