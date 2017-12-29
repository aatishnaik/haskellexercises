module LineNumbers where
import Data.Char
import Data.List

numlist n = map (\i -> show i) [1..n]

addnum :: [String] -> [(String,String)]
addnum strlist = zip (numlist (length strlist)) (strlist)

main :: IO()
main = print (addnum ["abc","bbc","as","sa","ll"])
--main = print (numlist 20)