module Chapter3.DateAdd4 where
import Data.List
import Data.Char

data Date = MkDate {
  day :: Int
, month :: Int
, year :: Int
} deriving (Eq, Show, Ord)

addDays :: Date -> Int -> Date
addDays (MkDate day month year) dys =
    let
        totaldays = day+dys
        monthlist = if (year `mod` 4) == 0 then [31,29,31,30,31,30,31,31,30,31,30,31] else [31,28,31,30,31,30,31,31,30,31,30,31]
        marr = [month..((month + (totaldays `div` 30)))]
        msum = sum (map (\x->(monthlist!!(x-1))) marr)
        avg = (fromIntegral msum) `div` (fromIntegral (length marr))
        xtraday = totaldays `div` avg
    in MkDate
    {
        day = if totaldays <= (monthlist!!month) then totaldays else (totaldays `mod` avg),
        month =
            if (totaldays > (monthlist!!month)) && ((month + xtraday) <= 12) then (month + xtraday)
            else if ((day+dys) > avg) && ((month + xtraday) > 12)
                then (month + xtraday) `mod` 12
            else month,
        year = if (month + (totaldays `div` avg)) > 12
            then year+(month + (totaldays `div` avg) `div` 12)
            else year
    }