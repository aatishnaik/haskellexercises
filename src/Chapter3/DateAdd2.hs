module Chapter3.DateAdd2 where
import Data.List
import Data.Char

data Day = MkDay Int deriving Show
data Month = MkMonth Int deriving Show
data Year = MkYear Int deriving Show


addDays :: (Day, Month, Year) -> Int -> (Day, Month, Year)
addDays (MkDay day,MkMonth month,MkYear year) dys =
    let totaldays = day+dys
        monthlist = if (year `mod` 4) == 0 then [31,29,31,30,31,30,31,31,30,31,30,31] else [31,28,31,30,31,30,31,31,30,31,30,31]
        xtraday = totaldays `div` 30
        in(
            if totaldays <= (monthlist!!month) then MkDay totaldays else MkDay (totaldays `mod` 30),
            if (totaldays > (monthlist!!month)) && ((month + xtraday) <= 12) then MkMonth (month + xtraday)
                else if ((day+dys) > 30) && ((month + xtraday) > 12)
                    then MkMonth ((month + xtraday) `mod` 12)
                else MkMonth month,
            if (month + (totaldays `div` 30)) > 12
                then MkYear (year+((month + xtraday) `mod` 12))
                else MkYear year)