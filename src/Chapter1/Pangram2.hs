--http://exercism.io/exercises/haskell/pangram/readme
module Chapter1.Pangram2 where
import Data.Char
import Data.List

clowercase :: String -> String
clowercase [x] = [(toLower x)]
clowercase str = [(toLower (head str))] ++ clowercase (tail str)

del :: String -> String -> String
del [c] alphabets = delete c alphabets
del str alphabets = delete (head str) (del (tail str) alphabets)

checkpan str = if (del (clowercase str) ['a'..'z']) == ""
    then True
    else False

main :: IO()
main = print (checkpan "Five quacking Zephyrs jolt my wax bed")