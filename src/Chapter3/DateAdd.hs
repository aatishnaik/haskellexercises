module Chapter3.DateAdd where
import Data.List
import Data.Char
checkLeap :: Int -> Bool
checkLeap yr = if (((yr `mod` 100) /= 0) && ((yr `mod` 4) == 0)) || ((yr `mod` 400) == 0)
        then True
        else False

getOffset :: Int -> Int -> Int -> Int
getOffset d m y =
    let monthlist = monthGetList y
    in d + (sum (take (m-1) monthlist))

addDays :: Int -> Int -> Int -> Int -> (Int,Int,Int)
addDays d m y x = 
    let offset = (getOffset d m y) + x
        (y2,mOffset) = addYr y offset
        (m2,dOffset) = addMon y2 mOffset
    in (dOffset,m2,y2)

addMon :: Int -> Int -> (Int,Int)
addMon yr offset =
    let monthlist = monthGetList yr
    in foldl' (\(month,off) mday ->
            if (off-mday) > 0
                then (month+1,off-mday)
            else (month,off)
        ) (1,offset) monthlist

addYr :: Int -> Int -> (Int,Int)
addYr yr offset =
    let yrd = if checkLeap yr then 366 else 365
    in if (offset-yrd) > 0
        then addYr (yr+1) (offset-yrd)
        else (yr,offset)

monthGetList :: Int -> [Int]
monthGetList yr = if checkLeap yr then [31,29,31,30,31,30,31,31,30,31,30,31] else [31,28,31,30,31,30,31,31,30,31,30,31]