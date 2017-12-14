--http://exercism.io/exercises/haskell/pangram/readme
import Data.Char
import Data.List

createmap = ['a'..'z']
createmap1 = ['A'..'Z']
getnext x = createmap !! ((head (Data.List.elemIndices x createmap))+1)
getnext1 x = createmap1 !! ((head (Data.List.elemIndices x createmap1))+1)
checkp x _ 'Z'= True
checkp x 'z' _= True
checkp x y z= if (y `elem` x) || (z `elem` x)
    then checkp x (getnext y) (getnext1 z)
    else False

checkpan x = checkp x 'a' 'A'

main :: IO()
main = print (checkpan "Five quacking Zephyrs jolt my wax bed")