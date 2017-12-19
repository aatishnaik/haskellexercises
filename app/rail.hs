import Data.Char
import Data.List

diff key = (2 * (key - 1))

indexarr key str = [1,(diff key)+1..(length str)]
tostr :: Char -> String
tostr x = [x]
getstr :: Int -> String -> [Int] -> String
getstr _ _ [] = ""
getstr key str index = tostr(str!!((head index)-1)) ++ (getstr key str (tail index))
rail :: String -> Int -> String
rail str k = getstr k str (indexarr k str)
main :: IO()
main =
    print (rail "abcdefghijklmnopqrstuv" 2)