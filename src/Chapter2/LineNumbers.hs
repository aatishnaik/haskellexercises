module Chapter2.LineNumbers where
import Data.Char
import Data.List

numlist n = map (\i -> (show i)++":") [1..n]

--using recursion
add :: [String] -> [String] -> [String]
add [n] [x] = [n++x]
add numlist strlist = [(head numlist ++ head strlist)] ++ add (tail numlist) (tail strlist)
addnum :: [String] -> [String]
addnum strlist = add (numlist (length strlist)) strlist

getx (x,y)=x
gety (x,y)=y
--using foldl'
--addnum2 :: [String] -> [String]
addnum2 strlist = foldl' (\ x -> (getx x) ++ (gety x)) [] [((numlist (length strlist)),strlist)]

--using zip
addnum3 :: [String] -> [(String,String)]
addnum3 strlist = zip (numlist (length strlist)) (strlist)

--using zipWith
addnum4 :: [String] -> [String]
addnum4 strlist = zipWith (++) (numlist (length strlist)) strlist

main :: IO()
main = print (addnum2 1 ["abc","bbc","as","sa","ll"])
--main = print (numlist 20)