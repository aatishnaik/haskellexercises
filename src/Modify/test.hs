module Test where
data TrafficLight = Red | Yellow | Green
data List = MkList [Int]
instance Eq List where
    (==) (MkList x) (MkList y) = (length x) == (length y)

data Sign = Pos | Neg | Mul | Div
instance Eq Sign where
    (==) Pos Pos = True
    (==) Neg Neg = True
    (==) _ _ = False

instance Ord Sign where
    (>)Pos Neg = True
    (>)Neg Pos = False
    --(<)Pos Neg = False
    --(<)Neg Pos = True
    (>) _ _ = False
    --(<) _ _ = False

instance Show TrafficLight where  
    show Red = "Red light"  
    show Yellow = "Yellow light"  
    show Green = "Green light"  

data Suit = Club | Diamond | Heart | Spade
    deriving (Read,Show,Enum,Eq,Ord)

data CardValue = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
   deriving (Read,Show,Enum,Eq,Ord)

data Card = Card {value :: CardValue, 
                    suit :: Suit}
    deriving (Read,Show,Eq)

instance Ord Card where
      compare c1 c2 = compare (value c1, suit c1) (value c2, suit c2)
 
instance Enum Card where
      toEnum n  = let (v,s) = n`divMod`4 in Card (toEnum v) (toEnum s)
      fromEnum c = fromEnum (value c) * 4 + fromEnum (suit c)

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