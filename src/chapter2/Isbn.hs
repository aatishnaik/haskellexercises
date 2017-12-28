module Chapter2.Isbn where
import Data.Char
import Data.List

rmdash :: String -> String
rmdash str = filter (/='-') str

cformat str = if (False `elem` (map (\c -> (isDigit c)) (rmdash str))) || (length (rmdash str) /= 10)
    then False
    else True

checkisbn str = if (cformat str)
    then if ((foldl' (\val c -> (val+c)) 0 (map (\c -> (digitToInt c)) (rmdash str))) `mod` 11) == 0
        then True
        else False
    else False