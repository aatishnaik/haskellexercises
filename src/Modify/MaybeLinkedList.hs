module ADT.LinkedList where

data Node a = Element a (Node a)
    | Empty
    deriving (Eq, Show, Ord)

--simplify insertion. Inserts all elements from list to LL
createList ::  Eq a => [a] -> Node a -> Node a
createList list first = 
    case first of 
    Element _ _ -> if (list /= []) 
        then createList (init list) (Element (last list) first) 
        else first
    Empty -> createList (init list) (Element (last list) Empty)

showlist :: Node a -> [a] -> [a]
showlist ele arr = case ele of 
    Element val nxt -> (showlist nxt (arr++[val]))
    Empty -> arr

    --1
prepend :: Node a -> a -> Node a
prepend first value = case first of 
    Element _ _ -> Element value first
    Empty -> Element value Empty

instance Eq Node a where
    (==) (Element val (Node nxt)) (Element val1 (Node nxt1)) = val == val1 && nxt == nxt1
    (==) Empty Empty = True
    (==) _ _ == False

    --2
append ::  Eq a => Node a -> a -> Node a
append first value = case first of 
    Element val nxt -> if nxt == Empty
        then Element value Empty
        else 
            Element val (append nxt value)
    Empty -> Empty
    
    --3
insertAtPos :: Node a -> a -> Int -> Int -> Node a
insertAtPos first value pos cpos = case first of 
    Element val nxt -> if pos == (cpos+1)
        then Element value nxt
        else Element val (insertAtPos nxt value pos (cpos+1))
    Empty -> Empty
    
    --4
rmIndex :: Node a -> Int -> Int -> Node a
rmIndex ele pos cpos = case ele of 
    Element val nxt -> if (pos-2) == cpos
        then
            let Element _ n = nxt
            in Element val n
        else Element val (rmIndex nxt pos (cpos+1))
    Empty -> Empty
    
    --5
getLength :: Node a -> Int -> Int
getLength first cpos = case first of 
    Element _ nxt -> getLength nxt (cpos+1)
    Empty -> cpos
    
    --6
reverseList :: Node a -> Node a -> Node a
reverseList first new = case first of 
    Element val nxt -> reverseList nxt (Element val new)
    Empty -> new
    
    --7
getIndex :: Eq a => Node a -> a -> Int -> String
getIndex ele value pos = case ele of 
    Element val nxt -> if val==value
        then "Element found at " ++ show (pos+1)
        else getIndex nxt value (pos+1)
    Empty -> "Element not found"
    
    --8
breakList :: Node a -> Int -> (Node a,Node a)
breakList first pos = breakL first pos first 0
    
breakL :: Node a -> Int -> Node a -> Int -> (Node a,Node a)
breakL first pos f cpos = case first of 
    Element _ nxt -> if pos == cpos
        then (cropList f pos 0,first)
        else breakL nxt pos f (cpos+1)
    Empty -> (Empty,Empty)
    
cropList :: Node a -> Int -> Int -> Node a
cropList first pos cpos = case first of 
    Element val nxt -> if pos == cpos
        then Empty
        else Element val (cropList nxt pos (cpos+1))
    Empty -> Empty
    
    
    