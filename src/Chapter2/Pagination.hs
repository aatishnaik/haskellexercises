module Chapter2.Pagination where
import Data.Char
import Data.List

totalPage :: Int -> Int -> Int
totalPage ti ip = if (ti `mod` ip == 0)
    then (ti `div` ip)
    else (ti `div` ip)+1
getString :: Int -> Int -> Int -> String
getString fp lp cp = (foldl' (\ str x -> str ++" | "++ (show x)) "" [fp..cp]) ++ (foldl' (\ str x -> str ++" | "++ (show x)) "*" [(cp+1)..lp])

type ItemsPerPage = Int
type TotalItems = Int
type CurrentPage = Int
displayPagination :: TotalItems -> ItemsPerPage -> CurrentPage -> String
displayPagination ti ip cp = let tp = totalPage ti ip
    in if tp >= 8
        then if ((tp - cp) > 3) && (cp > 4)
            then "<<Prev | ..."++(getString (cp-3) (cp+3) cp )++" | ... | Next>>"
            else if (cp <= 4)
                then "<<Prev "++(getString 1 7 cp )++" |...| Next>>"
            else "<<Prev | ..."++(getString (tp - 7) tp cp )++" | Next>>"
        else "<<Prev "++(getString 1 tp cp)++" | Next>>"

main :: IO()
main = print (displayPagination 100 3 5)