import Data.Char
import Data.List

keydiff = [-1..10]
rail :: String -> Int -> Int -> [String]


rail x n i = if i < n
    then head x
    else
        