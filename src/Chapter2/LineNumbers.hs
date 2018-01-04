module Chapter2.LineNumbers where
import Data.Char
import Data.List
numList :: Int -> [String]
numList n = (map (\i -> (show i)++": ") [1..n])

--using recursion
add :: [String] -> [String] -> [String]
add [n] [x] = [n++x]
add numlist strlist = [(head numlist ++ head strlist)] ++ add (tail numlist) (tail strlist)
prefixLineNumbers :: [String] -> [String]
prefixLineNumbers strlist = add ((map (\i -> (show i)++": ") [1..(length strlist)])) strlist

getx (x,y)=x
gety (x,y)=y
--using foldl'
--prefixLineNumbers2 :: [String] -> [String]
prefixLineNumbers2 strlist = foldl' (\ y x -> y ++ (getx x) ++ (gety x)) [] (intersperse [(numList (length strlist),strlist)])

--using zip
prefixLineNumbers3 :: [String] -> [String]
prefixLineNumbers3 strlist = map (\(num,str) -> num++str) (zip (map (\i -> (show i)++": ") [1..(length strlist)]) (strlist))

--using zipWith
prefixLineNumbers4 :: [String] -> [String]
prefixLineNumbers4 strlist = zipWith (\ x y -> x ++ y) (map (\i -> (show i)++": ") [1..(length strlist)]) strlist

main :: IO()
main = print (prefixLineNumbers2 ["abc","bbc","as","sa",""])
--main = print (numlist 20)