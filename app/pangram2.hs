--http://exercism.io/exercises/haskell/pangram/readme
import Data.Char
import Data.List

getnext :: Char -> String -> Char
getnext x y= y !! ((head (Data.List.elemIndices x y))+1)

clowercase :: String -> String
clowercase str = map (toLower) str

del x "" z = ""
del x y z = delete (getnext x z) (del x y z)

main :: IO()
main = print (clowercase "Five quacking Zephyrs jolt my wax bed")