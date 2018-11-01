module ADT.Queue where

data Node = Element Int Node
    | Empty
    deriving (Eq, Show, Ord)

showlist :: Node -> [Int] -> [Int]
showlist ele arr = case ele of 
    Element val nxt -> (showlist nxt (arr++[val]))
    Empty -> arr

insert :: Node -> Int -> Node
insert first value = case first of 
    Element _ _ -> Element value first
    Empty -> Element value Empty

--simplify insertion. Inserts all elements from list to LL
createList :: [Int] -> Node -> Node
createList list first = 
    case first of 
    Element _ _ -> if (list /= []) 
        then createList (init list) (Element (last list) first) 
        else first
    Empty -> createList (init list) (Element (last list) Empty)

delete :: Node -> Node
delete first = case first of 
    Element val nxt -> if (nxt == Empty)
        then Empty
        else 
            Element val (delete nxt)
    Empty -> Empty