module Chapter3.DateAdd3 where
import Data.List
import Data.Char

data Date = MkDate Int Int Int deriving (Eq, Show, Ord)

checkLeap :: Int -> Bool
checkLeap yr = if (((yr `mod` 100) /= 0) && ((yr `mod` 4) == 0)) || ((yr `mod` 400) == 0)
        then True
        else False

getOffset :: Date -> Int
getOffset (MkDate d m y) =
    let monthlist = if checkLeap y then [31,29,31,30,31,30,31,31,30,31,30,31] else [31,28,31,30,31,30,31,31,30,31,30,31]
    in d + (sum (take (m-1) monthlist))

addDays :: Date -> Int -> Date
addDays (MkDate d m y) x = 
    let offset = (getOffset (MkDate d m y)) + x
        (y2,mOffset) = addYr (MkDate d m y) offset
        (m2,dOffset) = addMon (MkDate d m y) mOffset
    in (MkDate dOffset m2 y2)

addMon :: Date -> Int -> (Int,Int)
addMon (MkDate d m yr) offset =
    let monthlist = if checkLeap yr then [31,29,31,30,31,30,31,31,30,31,30,31] else [31,28,31,30,31,30,31,31,30,31,30,31]
    in foldl' (\(month,off) mday ->
            if (off-mday) > 0
                then (month+1,off-mday)
            else (month,off)
        ) (1,offset) monthlist

addYr :: Date -> Int -> (Int,Int)
addYr (MkDate d m yr) offset =
    let yrd = if checkLeap yr then 366 else 365
    in if (offset-yrd) > 0
        then addYr (MkDate d m (yr+1)) (offset-yrd)
        else (yr,offset)