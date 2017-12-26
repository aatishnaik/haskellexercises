--http://exercism.io/exercises/haskell/pangram/readme
import Data.Char
import Data.List

getnext :: Char -> String -> Char
getnext x y= y !! ((head (Data.List.elemIndices x y))+1)

clowercase :: String -> String
clowercase str = map (toLower) str

del x "" = ""
del x y = filter (/=x) (del (getnext x y) y)

main :: IO()
main = print (clowercase "Five quacking Zephyrs jolt my wax bed")