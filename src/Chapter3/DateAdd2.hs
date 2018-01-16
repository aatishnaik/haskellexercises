module Chapter3.DateAdd2 where
import Data.List
import Data.Char

data Day = MkDay Int deriving Show
data Month = MkMonth Int deriving Show
data Year = MkYear Int deriving Show


addDays :: (Day, Month, Year) -> Int -> (Day, Month, Year)
addDays (MkDay day,MkMonth month,MkYear year) dys =
    let tdays = day+dys
        monthlist = if (year `mod` 4) == 0 then [31,29,31,30,31,30,31,31,30,31,30,31] else [31,28,31,30,31,30,31,31,30,31,30,31]
        xday = tdays `div` 30
        in(
            if tdays <= (monthlist!!month) then MkDay tdays else MkDay (tdays `mod` 30),
            if (tdays > (monthlist!!month)) && ((month + xday) <= 12) then MkMonth (month + xday)
                else if ((day+dys) > 30) && ((month + xday) > 12)
                    then MkMonth ((month + xday) `mod` 12)
                else MkMonth month,
            if (month + (tdays `div` 30)) > 12 
                then MkYear (year+((month + xday) `mod` 12))
                else MkYear year)