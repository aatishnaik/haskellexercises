--http://exercism.io/exercises/haskell/pangram/readme
module Chapter1.Pangram where
import Data.Char
import Data.List

getnext :: Char -> Char
getnext x = ['a'..'z'] !! ((head (Data.List.elemIndices x ['a'..'z']))+1)

clowercase :: String -> String
clowercase str = map (toLower) str
checkp :: String -> Char -> Bool
checkp x 'z' = 'z' `elem` x
checkp x y = if (y `elem` x)
    then checkp x (getnext y)
    else False
checkpan :: String -> Bool
checkpan x = checkp (clowercase x) 'a'

main :: IO()
main = print (checkpan "Five quacking Zephyrs jolt my wax bed")