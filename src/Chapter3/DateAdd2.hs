module Chapter3.DateAdd2 where
import Data.List
import Data.Char

data Day = MkDay Int deriving (Eq, Show, Ord)
data Month = MkMonth Int deriving (Eq, Show, Ord)
data Year = MkYear Int deriving (Eq, Show, Ord)

checkLeap :: Year -> Bool
checkLeap (MkYear yr) = if (((yr `mod` 100) /= 0) && ((yr `mod` 4) == 0)) || ((yr `mod` 400) == 0)
        then True
        else False

getOffset :: Day -> Month -> Year -> Day
getOffset d (MkMonth m) y =
    let monthlist = monthGetList y
    in MkDay (dayopr (\x y -> x+y) d (MkDay (foldl' (\val (MkDay dy) -> val+dy) 0 (take (m-1) monthlist))))

addDays :: Day -> Month -> Year -> Day -> (Day,Month,Year)
addDays d m y x = 
    let offset = dayopr (\x y -> x+y) (getOffset d m y) x
        (y2,mOffset) = addYr y (MkDay offset)
        (m2,dOffset) = addMon y2 mOffset
    in (dOffset,m2,y2)

addMon :: Year -> Day -> (Month,Day)
addMon yr offset =
    let monthlist = monthGetList yr
    in foldl' (\(month,off) (mday) ->
            if (dayopr (\x y -> x-y) off (mday)) > 0
                then (MkMonth (monopr (\x y -> x+y) month (MkMonth 1)),MkDay (dayopr (\x y -> x-y) off mday))
            else (month,off)
        ) (MkMonth 1,offset) monthlist

addYr :: Year -> Day -> (Year,Day)
addYr yr offset =
    let yrd = if checkLeap yr then MkDay 366 else MkDay 365
    in if (dayopr (\x y -> x-y) offset yrd) > 0
        then ((MkYear (yropr (\x y -> x+y) yr (MkYear 1))),(MkDay (dayopr (\x y -> x-y) offset yrd)))
        else (yr,offset)

monthGetList :: Year -> [Day]
monthGetList yr = if checkLeap yr then [MkDay 31,MkDay 29,MkDay 31,MkDay 30,MkDay 31,MkDay 30,MkDay 31,MkDay 31,MkDay 30,MkDay 31,MkDay 30,MkDay 31] else [MkDay 31,MkDay 28,MkDay 31,MkDay 30,MkDay 31,MkDay 30,MkDay 31,MkDay 31,MkDay 30,MkDay 31,MkDay 30,MkDay 31]

dayopr :: (Int -> Int -> Int) -> Day -> Day -> Int
dayopr func (MkDay x) (MkDay y) = func x y
monopr :: (Int -> Int -> Int) -> Month -> Month -> Int
monopr func (MkMonth x) (MkMonth y) = func x y
yropr :: (Int -> Int -> Int) -> Year -> Year -> Int
yropr func (MkYear x) (MkYear y) = func x y