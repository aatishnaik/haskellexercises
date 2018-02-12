module MaybeC where

find :: (a->Bool) -> [a] -> Maybe a
find inpL inpList = case inpList of
    [] -> Nothing
    x: remList -> if (inpL x)
        then Just x
        else find inpL remList

elemIndex :: Eq a => a -> [a] -> Int -> Maybe Int
elemIndex elemnt inpList pos = case inpList of
    [] -> Nothing
    x: remList -> if (x == elemnt)
        then Just pos
        else elemIndex elemnt remList (pos+1)

findIndex :: (a -> Bool) -> [a] -> Int -> Maybe Int
findIndex inpL inpList pos = case inpList of
    [] -> Nothing
    x: remList -> if (inpL x)
        then Just pos
        else findIndex inpL remList (pos+1)