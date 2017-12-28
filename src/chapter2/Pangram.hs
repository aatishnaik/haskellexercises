module Chapter2.Pangram where
import Data.Char
import Data.List

pangram :: String -> Bool
pangram str = if (foldl' (\alphabets c -> delete (toLower c) alphabets) ['a'..'z'] str) == ""
    then True
    else False

--pangram2 :: String -> Bool
--pangram2 str = if (map (\c -> (filter (/=c) ['a'..'z'])) str) == ""
--    then True
--    else False
pangram2 :: String -> Bool
pangram2 str = if (foldl' (\test c -> test ++ (filter (==c) ['a'..'z'])) [] (toLow str)) == (filter (/=' ') (toLow str))
    then True
    else False

toLow str = map (toLower) str

pangram3 :: String -> Bool
pangram3 str = if any (False==) (map (\c -> (any (c==) (toLow str))) ['a'..'z'])
    then False
    else True