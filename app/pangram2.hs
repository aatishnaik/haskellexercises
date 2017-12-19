--http://exercism.io/exercises/haskell/pangram/readme
import Data.Char
import Data.List

getnext :: Char -> String -> Char
getnext x y= y !! ((head (Data.List.elemIndices x y))+1)

clowercase :: String -> String
clowercase str = map (toLower) str

del :: String -> String -> String
del [x] y = delete x y
del x y = delete (head x) (del (tail x) y)

checkpan str = if (del (clowercase str) ['a'..'z']) == ""
    then True
    else False
--del x "" z = ""
--del x y z = delete (getnext x z) (del x y z)

main :: IO()
main = print (checkpan "Five quacking Zephyrs jolt my wax bed")