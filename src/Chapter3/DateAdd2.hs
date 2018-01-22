module Chapter3.DateAdd2 where
import Data.List
import Data.Char

data Day = MkDay Int deriving (Eq, Show, Ord)
data Month = MkMonth Int deriving (Eq, Show, Ord)
data Year = MkYear Int deriving (Eq, Show, Ord)

addDays :: (Day, Month, Year) -> Int -> (Day, Month, Year)
addDays (MkDay day,MkMonth month,MkYear year) dys =
    let totaldays = day+dys
        monthlist = if (year `mod` 4) == 0 then [31,29,31,30,31,30,31,31,30,31,30,31] else [31,28,31,30,31,30,31,31,30,31,30,31]
        marr = [month..((month + (totaldays `div` 30)))]
        msum = sum (map (\x->(monthlist!!(x-1))) marr)
        avg = (fromIntegral msum) `div` (fromIntegral (length marr))
        xtraday = totaldays `div` avg
        in(
            if totaldays <= (monthlist!!month) then MkDay totaldays else MkDay (totaldays `mod` avg),
            if (totaldays > (monthlist!!month)) && ((month + xtraday) <= 12) then MkMonth (month + xtraday)
                else if ((day+dys) > avg) && ((month + xtraday) > 12)
                    then MkMonth ((month + xtraday) `mod` 12)
                else MkMonth month,
            if (month + (totaldays `div` avg)) > 12
                then MkYear (year+((month + xtraday) `mod` 12))
                else MkYear year)