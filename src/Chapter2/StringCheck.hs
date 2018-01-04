module Chapter2.StringCheck where
import Data.List
import Data.Char

chcheck :: String -> Char -> Bool
chcheck str c = c `elem` str

strcheck :: String -> String -> String
strcheck str str2 = filter (chcheck str2) str
stringcheck :: String -> String -> Bool
stringcheck str1 str2 = str1 == (strcheck str1 str2)

--using foldl'
stringcheck2 :: String -> String -> Bool
stringcheck2 str1 str2 = (foldl' (\temp c -> if (chcheck str2 c) then temp++[c] else temp++"") [] str1) == str1

main :: IO()
main = print (stringcheck2 "aabc" "abcde")