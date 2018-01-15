module Chapter3.DateAdd where
import Data.List
import Data.Char

--version1
addDays :: (Int, Int, Int) -> Int -> (Int, Int, Int)
addDays (day,month,year) dys = 
    let tdays = day+dys
        monthlist = if (year `mod` 4) == 0 then [31,29,31,30,31,30,31,31,30,31,30,31] else [31,28,31,30,31,30,31,31,30,31,30,31]
    in(
        if tdays <= (monthlist!!(month-1)) then tdays else (tdays `mod` 30)+1,
        if (tdays > (monthlist!!(month-1))) && ((month + (tdays `div` 30)) <= 12) 
            then month + (tdays `div` 30)
            else if ((day+dys) >(monthlist!!(month-1))) && ((month + (tdays `div` 30)) > 12)
                then ((month + (tdays `div`30)) `mod` 12)+1
            else month,
        if (month + (tdays `div`30)) > 12 
            then year+(((month + (tdays `div`30)) `mod` 12)+1)
            else year)


--version2
data Day = MkDay Int deriving Show
data Month = MkMonth Int deriving Show
data Year = MkYear Int deriving Show

addDays2 :: (Day, Month, Year) -> Day -> (Day, Month, Year)
addDays2 (day,month,year) dys =
    let tdays = day+dys
        monthlist = if (year `mod` 4) == 0 then [31,29,31,30,31,30,31,31,30,31,30,31] else [31,28,31,30,31,30,31,31,30,31,30,31]
        xmdays = tdays `div` 30
        in(
        if tdays <= (monthlist!!month) then tdays else tdays `mod` 30,
        if (tdays > (monthlist!!month)) && ((month + ) <= 12) then month + xmdays
            else if ((day+dys) > 30) && ((month + xmdays) > 12)
                then ((month + xmdays) `mod` 12)
            else month,
        if (month + xmdays) > 12 
            then year+((month + xmdays) `mod` 12)
            else year)