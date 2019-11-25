module AlgebraicDatatypes.ConstructingAndDeconstructingValues where

--equivalent to ()
data GuessWhat = 
    Chickenbutt deriving (Eq, Show)

data Id a =
    MkId a 
    deriving (Eq, Show)

data Product a b = 
    Product a b
    deriving (Eq, Show)
    
data Sum a b = 
    First a
  | Second b 
  deriving (Eq, Show)
  
data RecordProduct a b =
    RecordProduct { pfirst :: a
                  , psecond::b }
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

-- The same as Farmhouse
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

--Alternately
type Animal' = 
    Sum CowInfo (Sum PigInfo SheepInfo)

data Twitter = 
    Twitter deriving (Eq, Show)

data AskFm = 
    AskFm deriving (Eq, Show)

socialNetwork :: Sum Twitter AskFm
socialNetwork = First Twitter

myRecord :: RecordProduct Integer Float
myRecord = RecordProduct 42 0.0000001

myRecord' :: RecordProduct Integer Float
myRecord' = 
    RecordProduct { pfirst = 42 
                  , psecond = 0.0000001 }

data OperatingSystem = 
    GnuPlusLinux
  | OpenBSDPlusNevermindJustBSDStill
  | Mac
  | Windows
  deriving(Eq,Show)
 
data ProgLang = 
    Haskell
  | Agda
  | Idris
  | PureScript
  deriving(Eq,Show)
  
data Programmer = 
    Programmer { os :: OperatingSystem
               , lang :: ProgLang } 
               deriving(Eq,Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems = 
    [ GnuPlusLinux
    , OpenBSDPlusNevermindJustBSDStill
    , Mac
    , Windows
    ]

allLanguages :: [ProgLang]
allLanguages = 
    [Haskell , Agda , Idris , PureScript]

allProgrammers :: [Programmer]
allProgrammers = [Programmer os' lang' | os' <- allOperatingSystems, lang' <- allLanguages]

allProgrammers' :: [Programmer]
allProgrammers' = Programmer <$> allOperatingSystems <*> allLanguages

allProgrammers'' :: [Programmer]
allProgrammers'' = do 
    os' <- allOperatingSystems 
    lang' <- allLanguages
    return $ Programmer os' lang'

allProgrammers3 :: [Programmer]
allProgrammers3 = allOperatingSystems >>= \os' -> 
                    allLanguages >>= \lang' -> 
                    return $ Programmer os' lang'

allProgrammers4 :: [Programmer]
allProgrammers4 = cartProd allOperatingSystems allLanguages
    where cartProd :: [OperatingSystem] -> [ProgLang] -> [Programmer]
          cartProd _ []=[]
          cartProd [] _ = []
          cartProd (x:xs) (y:ys) = Programmer x y : (cartProd [x] ys ++ cartProd xs ys  ++ cartProd xs [y])
    