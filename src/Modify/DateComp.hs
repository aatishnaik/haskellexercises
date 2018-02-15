module DateComp where

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

instance Ord DayOfWeek where
    compare Mon Mon = EQ
    compare Tue Tue = EQ
    compare Weds Weds = EQ
    compare Thu Thu = EQ
    compare Fri Fri = EQ
    compare Sat Sat = EQ
    compare Sun Sun= EQ
    compare Fri _= GT
    compare _ Fri= LT
    compare _ _= LT