import Control.Applicative (Applicative(..))
type Name = String

data Employee = Employee Name String
  deriving (Show)

names = ["Joe", "Ryan", "Marc"]
phones = ["123", "34234", "3423432"]

newtype ZipList a = ZipList { getZipList :: [a] }
  deriving (Eq, Show,  Functor)

instance Applicative ZipList where
  pure = ZipList . repeat
  ZipList fs <*> ZipList xs = ZipList (zipWith ($) fs xs)

data BigRecord = BR { getName :: Name
                    , getSSN  :: String
                    , getSalary :: Integer
                    , getPhone :: String
                    , getLicensePlate :: String
                    , getNumSickDays :: Int}
                    deriving (Show)

r = BR "Brent" "XXX-XX-XXX4" 600000000 "555-1234" "JGX-55T3" 2

getEmp :: BigRecord -> Employee
getEmp = Employee <$> getName <*> getPhone

pair :: Applicative f => f a -> f b -> f (a, b)
pair = liftA2 (,)

(*>) :: Applicative f => f a -> f b -> f b
fa *> fb = fb

mapA :: Applicative f => (a -> f b) -> ([a] -> f [b])
mapA fn (x:xs) = undefined

sequenceA :: Applicative f => [f a] -> f [a]
sequenceA (fx:fxs) = 
