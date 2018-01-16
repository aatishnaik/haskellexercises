module Chapter3.DateAdd where
import Data.List
import Data.Char

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