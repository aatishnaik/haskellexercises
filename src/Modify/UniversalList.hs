module UniLinkedList where

data Node a = Element a (Node a)
    | Empty

instance Eq a => Eq (Node a) where
    (==) (Element val nxt) (Element val1 nxt1) = val == val1 && nxt == nxt1
    (==) Empty Empty = True
    (==) _ _ = False
    (/=) (Element val nxt) (Element val1 nxt1) = val /= val1 || nxt /= nxt1
    (/=) Empty Empty = True
    (/=) _ _ = False

instance Ord a => Ord (Node a) where
    (>=) (Element val _) (Element val1 _) = val >= val1
    (>=) _ _ = False
    (>) (Element val _) (Element val1 _) = val > val1
    (>) _ _ = False
    (<=) (Element val _) (Element val1 _) = val <= val1
    (<=) _ _ = False
    (<) (Element val _) (Element val1 _) = val < val1
    (<) _ _ = False

instance Show a => Show (Node a) where
    show (Element val nxt) = show val ++" "++ show nxt
    show Empty = "Empty"

--simplify insertion. Inserts all elements from list to LL
fromList :: [a] -> Node a -> Node a
fromList list first = 
    case first of 
    Element _ _ -> case list of
        [] -> first
        _ -> fromList (init list) (Element (last list) first)
    Empty -> fromList (init list) (Element (last list) Empty)

tolist :: Node a -> [a] -> [a]
tolist ele arr = case ele of 
    Element val nxt -> (tolist nxt (arr++[val]))
    Empty -> arr

--1
prepend :: Node a -> a -> Node a
prepend first value = case first of 
    Element _ _ -> Element value first
    Empty -> Element value Empty

--2
append :: Node a -> a -> Node a
append first value = case first of 
    Element val nxt -> case nxt of
        Empty -> Element value Empty
        _ -> Element val (append nxt value)
    Empty -> Empty

--3
insertAtPos :: Node a -> a -> Int -> Node a
insertAtPos firstEle eleValue position = iPos firstEle eleValue position 0
            where iPos first value pos cpos = case first of 
                    Element val nxt -> if pos == (cpos+1)
                        then Element value nxt
                        else Element val (iPos nxt value pos (cpos+1))
                    Empty -> Empty

--4
removeIndex :: Node a -> Int -> Node a
removeIndex first bIndex = rmIndex first bIndex 0
            where rmIndex ele pos cpos = case ele of 
                    Element val nxt -> if (pos-2) == cpos
                        then
                            let Element _ n = nxt
                            in Element val n
                        else Element val (rmIndex nxt pos (cpos+1))
                    Empty -> Empty

--5
getLength :: Node a -> Int
getLength firstEle = gLen firstEle 0
        where gLen first cpos = case first of 
                Element _ nxt -> gLen nxt (cpos+1)
                Empty -> cpos

--6
reverseList :: Node a -> Node a
reverseList firstEle = revList firstEle Empty
            where revList first new = case first of 
                    Element val nxt -> revList nxt (Element val new)
                    Empty -> new

--7
getIndex :: Eq a => Node a -> a -> Int -> Maybe Int
getIndex ele value pos = case ele of 
    Element val nxt -> if val==value
        then Just (pos+1)
        else getIndex nxt value (pos+1)
    Empty -> Nothing

--8
breakList :: Node a -> Int -> (Node a,Node a)
breakList first pos = if pos < (getLength first) then breakL first pos first 0 else (Empty,Empty)
        where breakL first1 pos1 f cpos = case first1 of 
                Element _ nxt -> if pos1 == cpos
                    then (cropList f pos1 0,first1)
                    else breakL nxt pos1 f (cpos+1)
                Empty -> (Empty,Empty)
                where cropList firstEle positn cpositn = case firstEle of 
                        Element val nxt -> if positn == cpositn
                            then Empty
                            else Element val (cropList nxt positn (cpositn+1))
                        Empty -> Empty



