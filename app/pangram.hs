--http://exercism.io/exercises/haskell/pangram/readme
import Data.Char
import Data.List

createmap = ['a'..'z']
getnext x = createmap !! ((head (Data.List.elemIndices x createmap))+1)

checkp x 'z' = True
checkp x y = if (y `elem` x)
    then checkp x (getnext y)
    else False

checkpan x = checkp x 'a'

main :: IO()
main = do
    checkpan "abcdefghijklmnopqrstuvwxyzabc"