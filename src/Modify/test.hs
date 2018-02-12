module Test where

data List = MkList [Int]
instance Eq List where
    (==) (MkList x) (MkList y) = if (length x) == (length y) then True else False

data Sign = Pos | Neg | Mul | Div
instance Eq Sign where
    (==) Pos Pos = True
    (==) Neg Neg = True
    (==) _ _ = False

instance Ord Sign where
    (>)Pos Neg = True
    (>)Neg Neg = True
    (>) _ _ = False 


{--
data DayOfWeek = Mon | Tue | Weds | Thu | Fri | Sat | Sun
data Date = Date DayOfWeek Int
instance Eq DayOfWeek where
    (==) Mon Mon = True
    (==) Tue Tue = True
    (==) Weds Weds = True
    (==) Thu Thu = True
    (==) Fri Fri = True
    (==) Sat Sat = True
    (==) Sun Sun = True
    (==) _ _ = False

instance Eq Date where
    (==) (Date weekday dayOfMonth) (Date weekday' dayOfMonth') = weekday == weekday' && dayOfMonth == dayOfMonth'
--}