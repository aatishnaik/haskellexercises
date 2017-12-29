module Chapter1.Isbn where
import Data.List
import Data.Char

rmdash :: String -> String
rmdash str = if ('-' `elem` str)
    then delete '-' (rmdash (delete '-' str))
    else str

cdigit :: String -> Bool
cdigit [] = True
cdigit x = if isDigit (head x)
    then cdigit (tail x)
    else False

cformat :: String -> String
cformat str
    | (length str) > 10 = if cdigit (rmdash str)
        then rmdash str
        else ""
    | (length str) == 10 = if cdigit str
        then str
        else ""
    | otherwise = ""

checkmod :: String -> Int -> Int
checkmod x n
    |n > 0 = (digitToInt (x!!(10-n)) * n) + checkmod x (n-1)
    |otherwise = 0

checkisbn :: String -> Bool
checkisbn x = if (length (cformat x)) == 10
        then ((checkmod (cformat x) 10) `mod` 11) == 0
        else False

main :: IO()
main =
    print (checkisbn "3-598-2150-88")