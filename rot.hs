--http://exercism.io/exercises/haskell/rotational-cipher/readme
import Data.Char
import Data.List
merge :: [x] -> [x] -> [x]
merge [] [] = []
merge x [] = x
merge [] x = x
merge (x:xs) (y:ys) = x : y : merge xs ys

createmap = ['a'..'z']
geti x xs k= createmap !! ((head (Data.List.elemIndices x xs))+k)

toString :: Char -> String
toString c = [c]
rotation x k = map (toString(geti (head x) x k)) x
main :: IO()
main = do
    print (rotation "bbbb" 3)