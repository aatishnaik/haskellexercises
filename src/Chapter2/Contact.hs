
module Chapter2.Contact where
import Data.Char
import Data.List

groupAll :: [String] -> [[String]]
groupAll str = groupBy (\ a b -> ((head a) == (head b))) (sort str)

--generateArray :: [String] -> (Char,[String])
--generateArray str = 

main :: IO()
main = print (groupAll ["abc","bbc","abab","aaaa"])