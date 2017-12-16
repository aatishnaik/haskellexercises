--http://exercism.io/exercises/haskell/pangram/readme
import Data.Char
import Data.List

getnext :: String -> Char
getnext str = str !! ((head (Data.List.elemIndices (head str) str))+1)

clowercase :: String -> String
clowercase str = map (toLower) str

del "" = ""
del x = del (delete (head x) ['a'..'z'])

checkpan :: String -> Bool
checkpan x = if (del x) == 0

main :: IO()
main = print (checkpan "Five quacking Zephyrs jolt my wax bed")