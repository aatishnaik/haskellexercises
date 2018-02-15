module Test where

data TrafficLight = Red | Yellow | Green
data List = MkList [Int]

instance Eq List where
    (==) (MkList x) (MkList y) = (length x) == (length y)

instance Show TrafficLight where  
    show Red = "Red light"  
    show Yellow = "Yellow light"  
    show Green = "Green light"  
