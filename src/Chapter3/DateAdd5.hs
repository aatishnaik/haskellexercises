module Chapter3.DateAdd5 where
import Data.List
import Data.Char

data Date = MkDate {
  day :: Int
, month :: Int
, year :: Int
} deriving (Eq, Show, Ord)

checkLeap :: Int -> Bool
checkLeap yr = if (((yr `mod` 100) /= 0) && ((yr `mod` 4) == 0)) || ((yr `mod` 400) == 0)
        then True
        else False

getOffset :: Date -> Int
getOffset date =
    let monthlist = monthGetList date
    in (getDay date )+ (sum (take ((getMonth date)-1) monthlist))

addDays :: Date -> Int -> Date
addDays date x = 
    let offset = (getOffset date) + x
        (y2,mOffset) = addYr date offset
        (m2,dOffset) = addMon date mOffset
    in (MkDate dOffset m2 y2)

addMon :: Date -> Int -> (Int,Int)
addMon date offset =
    let monthlist = monthGetList date
    in foldl' (\(month,off) mday ->
            if (off-mday) > 0
                then (month+1,off-mday)
            else (month,off)
        ) (1,offset) monthlist

addYr :: Date -> Int -> (Int,Int)
addYr date offset =
    let yrd = if checkLeap (getYear date) then 366 else 365
    in if (offset-yrd) > 0
        then addYr (incYear date) (offset-yrd)
        else (getYear date,offset)

    --accessor functons
getDay :: Date -> Int
getDay (MkDate day month year) = day

getMonth :: Date -> Int
getMonth (MkDate day month year) = month

getYear :: Date -> Int
getYear (MkDate day month year) = year

incYear :: Date -> Date
incYear (MkDate day month year) = (MkDate day month (year+1))

monthGetList :: Date -> [Int]
monthGetList (MkDate d m yr) = if checkLeap yr then [31,29,31,30,31,30,31,31,30,31,30,31] else [31,28,31,30,31,30,31,31,30,31,30,31]