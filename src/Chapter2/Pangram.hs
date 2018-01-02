module Chapter2.Pangram where
import Data.Char
import Data.List

pangram :: String -> Bool
pangram str = (foldl' (\alphabets c -> delete (toLower c) alphabets) ['a'..'z'] str) == ""

pangram2 :: String -> Bool
pangram2 str = (foldl' (\test c -> test ++ (filter (==c) ['a'..'z'])) [] (toLow str)) == (filter (/=' ') (toLow str))

toLow str = map (toLower) str

pangram3 :: String -> Bool
pangram3 str = (not)(any (False==) (map (\c -> (any (c==) (toLow str))) ['a'..'z']))
main :: IO()
main = print (pangram "abcdefghijklmnopqrstuvwxyz")