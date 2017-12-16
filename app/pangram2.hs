--http://exercism.io/exercises/haskell/pangram/readme
import Data.Char
import Data.List

getnext :: Char -> String -> Char
getnext x str = str !! ((head (Data.List.elemIndices x str))+1)

clowercase :: String -> String
clowercase str = map (toLower) str

checkp :: String -> Char -> Bool
checkp x 'z' = 'z' `elem` x
checkp x y = if (y `elem` x)
    then checkp x (getnext y)
    else False

cpan c x 

checkpan :: String -> Bool
checkpan x = checkp (clowercase x) 'a'

main :: IO()
main = print (checkpan "Five quacking Zephyrs jolt my wax bed")