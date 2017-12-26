module Chapter1.Isbn where
import Data.List
import Data.Char
rmdash :: String -> String
rmdash x = delete '-' (delete '-' (delete '-' x))

cformat :: String -> String
cformat x
    | (length x) == 13 = if cdigit (rmdash x)
        then rmdash x
        else ""
    | (length x) == 10 = if cdigit x
        then x
        else ""
    | otherwise = ""

cdigit :: String -> Bool
cdigit [] = True
cdigit x = if isDigit (head x)
    then cdigit (tail x)
    else False

checkmod :: String -> Int -> Int
checkmod x n
    |n > 0 = (digitToInt (x!!(10-n)) * n) + checkmod x (n-1)
    |otherwise = 0

checkisbn :: String -> Bool
checkisbn x = if (length (cformat x)) == 10
        then ((checkmod x 10) `mod` 11) == 0
        else False

main :: IO()
main =
    print (checkisbn "3-598021508-8")