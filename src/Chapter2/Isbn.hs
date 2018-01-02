module Chapter2.Isbn where
import Data.Char
import Data.List

rmdash :: String -> String
rmdash str = filter (/='-') str
cformat :: String -> Bool
cformat str = (not) (False `elem` (map (\c -> (isDigit c)) (rmdash str))) || (length (rmdash str) /= 10)
checkisbn :: String -> Bool
checkisbn str = if (cformat str)
    then ((foldl' (\val c -> (val+c)) 0 (map (\c -> (digitToInt c)) (rmdash str))) `mod` 11) == 0
    else False

--using zip
checkisbn2 :: String -> Bool
checkisbn2 str = if (cformat str)
    then ((sum (zipWith (*) [1..10] (map (\c -> (digitToInt c)) (rmdash str)))) `mod` 11) == 0
    else False

main :: IO()
main = print (checkisbn2 "3-598-21508-8")