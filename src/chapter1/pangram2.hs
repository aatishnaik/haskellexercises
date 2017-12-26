--http://exercism.io/exercises/haskell/pangram/readme
import Data.Char
import Data.List

clowercase :: String -> String
clowercase [x] = [(toLower x)]
clowercase str = [(toLower (head str))] ++ clowercase (tail str)

del :: String -> String -> String
del [x] y = delete x y
del x y = delete (head x) (del (tail x) y)

checkpan str = if (del (clowercase str) ['a'..'z']) == ""
    then True
    else False

main :: IO()
main = print (checkpan "Five quacking Zephyrs jolt my wax bed")