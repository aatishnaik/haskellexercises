import Data.List
import Data.Char

chcheck :: String -> Char -> Bool
chcheck str c = c `elem` str
strcheck :: String -> String -> String
strcheck str str2 = filter (chcheck str2) str
stringcheck :: String -> String -> Bool
stringcheck str1 str2 = if str1 == (strcheck str1 str2)
    then True
    else False