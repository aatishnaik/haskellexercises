module Modify.LinkedList where

data Node = Element Int Node
    | Empty
    deriving (Eq, Show, Ord)

--simplify insertion. Inserts all elements from list to LL
fromList :: [Int] -> Node -> Node
fromList list first = 
    case first of 
    Element _ _ -> if (list /= []) 
        then fromList (init list) $ Element (last list) first
        else first
    Empty -> fromList (init list) $ Element (last list) Empty

tolist :: Node -> [Int] -> [Int]
tolist ele arr = case ele of 
    Element val nxt -> (tolist nxt (arr++[val]))
    Empty -> arr

--1
prepend :: Node -> Int -> Node
prepend first value = case first of 
    Element _ _ -> Element value first
    Empty -> Element value Empty
--2
append :: Node -> Int -> Node
append first value = case first of 
    Element val nxt -> if (nxt == Empty)
        then Element value Empty
        else 
            Element val (append nxt value)
    Empty -> Empty

--3
insertAtPos :: Node -> Int -> Int -> Int -> Node
insertAtPos first value pos cpos = case first of 
    Element val nxt -> if pos == (cpos+1)
        then Element value nxt
        else Element val (insertAtPos nxt value pos (cpos+1))
    Empty -> Empty

--4
rmIndex :: Node -> Int -> Int -> Node
rmIndex ele pos cpos = case ele of 
    Element val nxt -> if (pos-2) == cpos
        then
            let Element _ n = nxt
            in Element val n
        else Element val (rmIndex nxt pos (cpos+1))
    Empty -> Empty

--5
getLength :: Node -> Int -> Int
getLength first cpos = case first of 
    Element _ nxt -> getLength nxt (cpos+1)
    Empty -> cpos

--6
reverseList :: Node -> Node -> Node
reverseList first new = case first of 
    Element val nxt -> reverseList nxt (Element val new)
    Empty -> new

--7
getIndex :: Node -> Int -> Int -> String
getIndex ele value pos = case ele of 
    Element val nxt -> if val==value
        then "Element found at "++(show (pos+1))
        else getIndex nxt value (pos+1)
    Empty -> "Element not found"

--8
breakList :: Node -> Int -> (Node,Node)
breakList first pos = breakL first pos first 0

breakL :: Node -> Int -> Node -> Int -> (Node,Node)
breakL first pos f cpos = case first of 
    Element _ nxt -> if pos == cpos
        then (cropList f pos 0,first)
        else breakL nxt pos f (cpos+1)
    Empty -> (Empty,Empty)

cropList :: Node -> Int -> Int -> Node
cropList first pos cpos = case first of 
    Element val nxt -> if pos == cpos
        then Empty
        else Element val (cropList nxt pos (cpos+1))
    Empty -> Empty


