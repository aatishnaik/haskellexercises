import Data.List
import Data.Char

data Customer = MkCustomer
  { customerId :: Int
  , customerFirstName :: String
  , customerLastName :: String
  , customerEmail :: String
  , customerBirthYear :: Int
  , customerCity :: String
  , customerCountry :: String
  } deriving (Eq, Show, Ord)

removeSpaces :: String -> String
removeSpaces s = dropWhileEnd (\x -> isSpace x) (dropWhile (\x -> isSpace x) s)

cleanupName :: Customer -> Customer
cleanupName MkCustomer{customerId=cid, customerFirstName=fn, customerLastName=ln, customerEmail=e, customerBirthYear=by, customerCity=cy, customerCountry=co} =
  MkCustomer
    { customerId = cid
    , customerFirstName = (removeSpaces fn)
    , customerLastName = (removeSpaces ln)
    , customerEmail = e
    , customerBirthYear = by
    , customerCity = cy
    , customerCountry = co
    }
{--module Chapter3.Test where
import Data.List

data TimeOfDay = MkTimeOfDay Int Int deriving (Eq, Show, Ord)
data Customer = Customer
  { customerId :: Int
  , customerFirstName :: String
  , customerLastName :: String
  , customerEmail :: String
  , customerBirthYear :: Int
  , customerCity :: String
  , customerCountry :: String
  } deriving (Eq, Show, Ord)
calculateEndTime :: TimeOfDay -> Int -> TimeOfDay
calculateEndTime (MkTimeOfDay hr mn) durationInMins =
  let (addHour, finalMins) = divMod (mn + durationInMins) 60
  in MkTimeOfDay (hr + addHour) finalMins-}
