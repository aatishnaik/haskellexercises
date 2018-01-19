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