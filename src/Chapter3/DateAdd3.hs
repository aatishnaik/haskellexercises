module Chapter3.DateAdd3 where
import Data.List
import Data.Char
data Date = MkDate Int Int Int deriving Show

addDays :: Date -> Int -> Date
addDays (MkDate day month year) dys = 
    let 
        tdays = day+dys
        monthlist = if (year `mod` 4) == 0 then [31,29,31,30,31,30,31,31,30,31,30,31] else [31,28,31,30,31,30,31,31,30,31,30,31]
        xday = tdays `div` 30
        day2 = if tdays <= (monthlist!!month) then tdays else (tdays `mod` 30)
        month2 = 
            if (tdays > (monthlist!!month)) && ((month + xday) <= 12) then (month + xday)
            else if ((day+dys) > 30) && ((month + xday) > 12)
                then (month + xday) `mod` 12
            else month
        year2 = if (month + (tdays `div` 30)) > 12 
            then year+(month + (tdays `div`30) `div` 12)
            else year
    in (MkDate day2 month2 year2)