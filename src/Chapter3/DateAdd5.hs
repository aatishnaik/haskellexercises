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
        tdays = (getDay date)+dys
        monthlist = if ((getYear date) `mod` 4) == 0 then [31,29,31,30,31,30,31,31,30,31,30,31] else [31,28,31,30,31,30,31,31,30,31,30,31]
        xday = tdays `div` 30
    in MkDate
    {
        day = if tdays <= (monthlist!!(getMonth date)) then tdays else (tdays `mod` 30),
        month = 
            if (tdays > (monthlist!!(getMonth date))) && ((getMonth date + xday) <= 12) then (getMonth date + xday)
            else if (((getDay date)+dys) > 30) && (((getMonth date) + xday) > 12)
                then ((getMonth date) + xday) `mod` 12
            else (getMonth date),
        year = if ((getMonth date)+ (tdays `div` 30)) > 12 
            then (getYear date)+((getMonth date) + (tdays `div`30) `div` 12)
            else (getYear date)
    }

    --accessor functons
getDay :: Date -> Int
getDay (MkDate day month year) = day

getMonth :: Date -> Int
getMonth (MkDate day month year) = month

getYear :: Date -> Int
getYear (MkDate day month year) = year