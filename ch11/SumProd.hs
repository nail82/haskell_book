module SumProd where

data GuessWhat =
               Chickenbutt deriving (Eq, Show)

data Id a =
          MkId a deriving (Eq, Show)

-- Sum can only be First or Second
data Sum a b =
    First a
    | Second b
    deriving (Eq, Show)

data Product a b =
                 Product a b deriving (Eq, Show)

data RecordProduct a b =
    RecordProduct { pfirst :: a
                  , psecond :: b }
    deriving (Eq, Show)

newtype NumCow =
    NumCow Int
    deriving (Eq, Show)

newtype NumPig =
    NumPig Int
    deriving (Eq, Show)

newtype NumSheep =
    NumSheep Int
    deriving (Eq, Show)

data Farmhouse =
    Farmhouse NumCow NumPig
    deriving (Eq, Show)

type Farmhouse' = Product NumCow NumPig

data BigFarmhouse =
    BigFarmhouse NumCow NumPig NumSheep
    deriving (Eq, Show)

type BigFarmhouse' =
    Product NumCow (Product NumPig NumSheep)

type Name = String
type Age = Int
type LovesMud = Bool

type PoundsOfWool = Int

data CowInfo =
    CowInfo Name Age
    deriving (Eq, Show)

data PigInfo =
    PigInfo Name Age LovesMud
    deriving (Eq, Show)

data SheepInfo =
    SheepInfo Name Age PoundsOfWool
    deriving (Eq, Show)

data Animal =
    Cow CowInfo
    | Pig PigInfo
    | Sheep SheepInfo
    deriving (Eq, Show)

-- Animal' is either First CowInfo or Second First PigInfo or Second Second SheepInfo
type Animal' =
    Sum CowInfo (Sum PigInfo SheepInfo)

idInt :: Id Integer
idInt = MkId 2

type Awesome = Bool

person :: Product Name Awesome
person = Product "Simon" True

data Twitter =
    Twitter deriving (Eq, Show)

data AskFm =
    AskFm deriving (Eq, Show)

socialNetwork :: SN
socialNetwork = First Twitter :: SN

type SN = Sum Twitter AskFm

socialNetwork' :: SN
socialNetwork' = Second AskFm :: SN

-- Exercise pg 430
data OperatingSystem =
                     GnuPlusLinux
                     | OpenBSD
                     | Mac
                     | Windows
                     deriving (Eq, Show)

data ProgLang =
              Haskell
              | Agda
              | Idris
              | PureScript
              deriving (Eq, Show)

data Programmer =
                Programmer { os :: OperatingSystem
                           ,lang :: ProgLang }
                deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
    [ GnuPlusLinux
    , OpenBSD
    , Mac
    , Windows
    ]

allLanguages :: [ProgLang]
allLanguages =
    [ Haskell
    , Agda
    , Idris
    , PureScript
    ]

allProgrammers :: [Programmer]
allProgrammers = map tupToProgrammer tups
                 where n      = length allOperatingSystems
                       oSes   = map repF allOperatingSystems
                       langs  = take n $ iterate id allLanguages
                       tups   = concat $ zipWith zipper oSes langs
                       zipper = \xs ys -> zip xs ys
                       repF   = \x -> take n $ repeat x


tupToProgrammer :: (OperatingSystem, ProgLang) -> Programmer
tupToProgrammer (o, p) = Programmer o p


-- Bottom Record, bad juju
-- partialProg :: Programmer
-- partialProg = Programmer { os = GnuPlusLinux }

-- Incremental data type construction
data ThereYet =
              There Float Int Bool
              deriving (Eq, Show)

nope :: Float -> Int -> Bool -> ThereYet
nope = There

notYet :: Int -> Bool -> ThereYet
notYet = nope 25.5

notQuite :: Bool -> ThereYet
notQuite = notYet 10

yeees :: ThereYet
yeees = notQuite False
