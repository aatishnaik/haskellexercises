import Data.List
import Data.Char

cformat :: String -> Bool
cformat x = if (length x) == 13
    then if (x!!1) == '-'
        then if (x!!5) == '-'
            then if (x!!11) == '-'
                then True
                else False
            else False
        else False
    else False

toInt :: Char -> Int
toInt x = digitToInt x

cdigit :: String -> Int -> Bool
cdigit x 1 = cdigit (tail x) 2
cdigit x 5 = cdigit (tail x) 6
cdigit x 11 = cdigit (tail x) 12
cdigit x 12 = if (isDigit (head x)) || ((head x) == 'X')
    then True
    else False
cdigit [] n = True
cdigit x n = if isDigit (head x)
    then cdigit (tail x) (n+1)
    else False

calsum :: String -> Int -> Int
calsum x 0 = 0
calsum [] n = 0
calsum x n 
    | n > 11 = ((toInt (head x)) * (n-1)) + (calsum (tail x) (n-1))
    | n > 5 =  if (n == 11)
        then calsum (tail x) (n-1)
        else ((toInt (head x)) * n) + (calsum (tail x) (n-1))
    | otherwise =  if (n == 1) || (n == 5)
        then calsum (tail x) (n-1)
        else ((toInt (head x)) * (n+1)) + (calsum (tail x) (n-1))

checkisbn :: String -> Bool
checkisbn x = if (cformat x) && (cdigit x 0)
    then ((calsum x 12) `mod` 11) == 0
    else False

main ::IO()
main = print (checkisbn "1-123012345-1")