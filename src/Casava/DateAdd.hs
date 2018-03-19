module Casava.DateAdd where
import Data.List
import Control.Lens
checkLeap :: Int -> Bool
checkLeap yr = if (((yr `mod` 100) /= 0) && ((yr `mod` 4) == 0)) || ((yr `mod` 400) == 0)
        then True
        else False

getOffset :: (Int,Int,Int) -> Int
getOffset date = (date^._1) + (sum (take ((date^._2)-1) monthlist))
    where monthlist = monthGetList (date^._3)

addDays :: (Int,Int,Int) -> Int -> (Int,Int,Int)
addDays date x = 
    let offset = (getOffset date) + x
        yMoff = addYr (date^._3) offset
        mDoff = addMon (yMoff^._1)  (yMoff^._2) 
    in ((mDoff^._2),(mDoff^._1),(yMoff^._1))

addMon :: Int -> Int -> (Int,Int)
addMon yr offset = foldl' (\(month,off) mday ->
        if (off-mday) > 0
            then (month+1,off-mday)
        else (month,off)
    ) (1,offset) monthlist
    where monthlist = monthGetList yr

addYr :: Int -> Int -> (Int,Int)
addYr yr offset = if (offset-yrd) > 0
    then addYr (yr+1) (offset-yrd)
    else (yr,offset)
    where yrd = if checkLeap yr then 366 else 365

monthGetList :: Int -> [Int]
monthGetList yr = if checkLeap yr then [31,29,31,30,31,30,31,31,30,31,30,31] else [31,28,31,30,31,30,31,31,30,31,30,31]