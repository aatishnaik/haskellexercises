module Chapter2.Double where
import Data.List
double :: [Int] -> [Int]
double list = map (\x -> x * 2) list
main ::IO()
main = print (double [1..10])