module ADT.Stack where
import Data.Char
import Data.List

data Node = Element Int Node
    | Empty
    deriving (Eq, Show, Ord)

showlist :: Node -> [Int] -> [Int]
showlist value arr = case value of 
    Element val nxt -> (showlist nxt (arr++[val]))
    Empty -> arr

pop :: Node -> (Int,Node)
pop first = case first of 
    Element val nxt -> (val,nxt)
    Empty -> (0,Empty)

push :: Node -> Int -> Node
push first value = case first of 
    Element _ _ -> Element value first
    Empty -> Element value Empty