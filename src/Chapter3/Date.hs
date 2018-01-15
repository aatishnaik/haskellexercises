module Chapter3.Date where
import Data.List
import Data.Char

data Date = MkDate {
  day :: Int
, month :: Int
, year :: Int
} deriving (Eq, Show, Ord)

add :: Date -> Int -> Date
add (MkDate {day = d, month = m, year = y}) dys = 
    let tdays = d+dys
    in MkDate
    {
        day = if (d+dys) < 30 then (d+dys) else (d+dys `mod` 30),
        month = 
            if (tdays > 30) && (month + (tdays `div` 30)) < 12 then month + (tdays `div`30)
            else if ((d+dys) > 30) && (month + (tdays `div`30)) > 12 
                then ((month + (tdays `div`30)) `mod` 12)
            else month,
        year = if (month + (tdays `div`30)) > 12 
            then y+(month + (tdays `div`30) `mod` 12)
            else year
    }