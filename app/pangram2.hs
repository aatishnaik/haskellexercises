--http://exercism.io/exercises/haskell/pangram/readme
import Data.Char
import Data.List

--lc :: String -> Char -> String
--lc [x] _ = toLower x
--lc str c = lc (tail str) (toLower (head str))

clowercase :: String -> String
clowercase [x] = [(toLower x)]
clowercase str = [(toLower (head str))] ++ clowercase (tail str)

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