module Modify.Sign where

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