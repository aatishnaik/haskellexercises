module Chapter2.LineNumbers where
import Data.Char
import Data.List

--using recursion
add :: [String] -> [String] -> [String]
add [n] [x] = [n++x]
add numlist strlist = [(head numlist ++ head strlist)] ++ add (tail numlist) (tail strlist)
prefixLineNumbers :: [String] -> [String]
prefixLineNumbers strlist = add ((map (\i -> (show i)++": ") [1..(length strlist)])) strlist

--using foldl'
prefixLineNumbers2 :: [String] -> [String]
prefixLineNumbers2 strlist = lines (foldl' (\str line -> str ++ [(intToDigit (head (elemIndices line strlist)))] ++ ": "++ line ++ "\n") [] strlist)

--using zip
prefixLineNumbers3 :: [String] -> [String]
prefixLineNumbers3 strlist = map (\(num,str) -> num++str) (zip (map (\i -> (show i)++": ") [1..(length strlist)]) (strlist))

--using zipWith
prefixLineNumbers4 :: [String] -> [String]
prefixLineNumbers4 strlist = zipWith (\ x y -> x ++ y) (map (\i -> (show i)++": ") [1..(length strlist)]) strlist

main :: IO()
main = print (prefixLineNumbers2 ["abc","bbc","as","sa",""])
--main = print (numlist 20)