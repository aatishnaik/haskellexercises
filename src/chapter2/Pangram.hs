module Chapter2.Pangram where
import Data.Char
import Data.List

pangram :: String -> Bool
pangram str = if (foldl' (\alphabets c -> delete (toLower c) alphabets) ['a'..'z'] str) == ""
    then True
    else False

pangram2 :: String -> Bool
pangram2 str = if (foldl' (\alphabets c -> delete (toLower c) alphabets) ['a'..'z'] str) == ""
    then True
    else False