module Chapter3.DateAdd where
import Data.List
import Data.Char

addDays :: (Int, Int, Int) -> Int -> (Int, Int, Int)
addDays (day,month,year) dys = 
    let tdays = day+dys
        monthlist = if (year `mod` 4) == 0 then [31,29,31,30,31,30,31,31,30,31,30,31] else [31,28,31,30,31,30,31,31,30,31,30,31]
        marr = [month..((month + (tdays `div` 31)))]
        msum = fromIntegral (sum (map (\x->(monthlist!!(x-1))) marr))
        len = (fromIntegral (length marr))
        avg = msum `div` len
    in(
        if tdays <= (monthlist!!(month-1)) then tdays else (tdays `mod` avg)+1,
        if (tdays > (monthlist!!(month-1))) && ((month + (tdays `div` avg)) <= 12) 
            then month + (tdays `div` avg)
            else if ((day+dys) >(monthlist!!(month-1))) && ((month + (tdays `div` avg)) > 12)
                then ((month + (tdays `div`avg)) `mod` 12)+1
            else month,
        if (month + (tdays `div`avg)) > 12 
            then year+(((month + (tdays `div` avg)) `mod` 12)+1)
            else year)