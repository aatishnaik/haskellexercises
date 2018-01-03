module Chapter2.Pagination where
import Data.Char
import Data.List

totalPage :: Int -> Int -> Int
totalPage ti ip = if (ti `mod` ip == 0)
    then (ti `div` ip)
    else (ti `div` ip)+1
getString :: Int -> Int -> Int -> String
getString fp lp cp = (foldl' (\ str x -> str ++" "++ (show x)) "<<Prev" [fp..cp]) ++ (foldl' (\ str x -> str ++" "++ (show x)) "*" [(cp+1)..lp])

type ItemsPerPage = Int
type TotalItems = Int
type CurrentPage = Int
displayPagination :: TotalItems -> ItemsPerPage -> CurrentPage -> String
--displayPagination :: Int -> Int -> Int -> String
displayPagination ti ip cp = if (totalPage ti ip) >= 8
    then if (((totalPage ti ip) - cp) >= 3) && (cp > 3)
        then (getString (cp-3) (cp+3) cp )++" Next>>"
        else if (cp < 3)
            then (getString 1 7 cp )++" Next>>"
        else (getString ((totalPage ti ip) - 7) (totalPage ti ip) cp )++" Next>>"
    else (getString 1 (totalPage ti ip) cp)++" Next>>"

main :: IO()
main = print (displayPagination 100 3 8)