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
  in MkTimeOfDay (hr + addHour) finalMins
  
let subjects = [MkSubjectName "English",MkSubjectName "Geography",MkSubjectName "Physics",MkSubjectName "Chemistry",MkSubjectName "Economics",MkSubjectName "Computer Science"]
let mksheet = (MkMarkSheet [(MkStudentName "Saurabh Nanda",[(MkSubjectName "English",MkSubjectMarks 84),(MkSubjectName "Chemisty",MkSubjectMarks 80),(MkSubjectName "Physics",MkSubjectMarks 95),(MkSubjectName "Geography",MkSubjectMarks 75)]),(MkStudentName "John Doe", [(MkSubjectName "Chemisty",MkSubjectMarks 80), (MkSubjectName "Physics",MkSubjectMarks 95), (MkSubjectName "Geography",MkSubjectMarks 75)]),(MkStudentName "Jane Doe", [(MkSubjectName "Chemisty",MkSubjectMarks 66), (MkSubjectName "Phsyics",MkSubjectMarks 33), (MkSubjectName "Geography",MkSubjectMarks 56)]),(MkStudentName "John Doe", [(MkSubjectName "Chemisty",MkSubjectMarks 90), (MkSubjectName "Economics",MkSubjectMarks 45), (MkSubjectName "Geography",MkSubjectMarks 56)]),(MkStudentName "Bahubali", [(MkSubjectName "Hindi",MkSubjectMarks 45), (MkSubjectName "Biology",MkSubjectMarks (-90)), (MkSubjectName "Geography",MkSubjectMarks (-75))]),(MkStudentName "Rajnikant", [(MkSubjectName "Tamil",MkSubjectMarks 110), (MkSubjectName "Biology",MkSubjectMarks 100), (MkSubjectName "Geography",MkSubjectMarks 100)])])

  -}
  