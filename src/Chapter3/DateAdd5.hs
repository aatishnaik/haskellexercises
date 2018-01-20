module Chapter3.DateAdd5 where
import Data.List
import Data.Char

data Date = MkDate {
      day :: Int
    , month :: Int
    , year :: Int
    } deriving (Eq, Show, Ord)

addDays :: Date -> Int -> Date
addDays date dys =
    let
        totaldays = (getDay date)+dys
        monthlist = if ((getYear date) `mod` 4) == 0 then [31,29,31,30,31,30,31,31,30,31,30,31] else [31,28,31,30,31,30,31,31,30,31,30,31]
        marr = [month..((month + (totaldays `div` 30)))]
        msum = sum (map (\x->(monthlist!!(x-1))) marr)
        avg = (fromIntegral msum) `div` (fromIntegral (length marr))
        xtraday = totaldays `div` avg
    in MkDate
    {
        day = if totaldays <= (monthlist!!(getMonth date)) then totaldays else (totaldays `mod` avf),
        month =
            if (totaldays > (monthlist!!(getMonth date))) && ((getMonth date + xtraday) <= 12) then (getMonth date + xtraday)
            else if (((getDay date)+dys) > avg) && (((getMonth date) + xtraday) > 12)
                then ((getMonth date) + xtraday) `mod` 12
            else (getMonth date),
        year = if ((getMonth date)+ (totaldays `div` avg)) > 12
            then (getYear date)+((getMonth date) + (totaldays `div` avg) `div` 12)
            else (getYear date)
    }

    --accessor functons
getDay :: Date -> Int
getDay (MkDate day month year) = day

getMonth :: Date -> Int
getMonth (MkDate day month year) = month

getYear :: Date -> Int
getYear (MkDate day month year) = year