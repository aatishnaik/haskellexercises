--http://exercism.io/exercises/haskell/rotational-cipher/readme
import Data.Char
import Data.List
merge :: [x] -> [x] -> [x]
merge [] [] = []
merge x [] = x
merge [] x = x
merge (x:xs) (y:ys) = x : y : merge xs ys
createmap :: String
createmap = ['a'..'z']
geti x xs k= createmap !! ((head (Data.List.elemIndices x xs))+k)

toString :: Char -> String
toString c = [c]
rotation [] k = ""
rotation x k = (toString(geti (head x) x k)) ++(rotation (tail x) k)
main :: IO()
main = do
    print (rotation "bbbb" 3)