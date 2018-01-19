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
type NumOfPagesToDisplay = Int

--part 1
displayPagination :: TotalItems -> ItemsPerPage -> CurrentPage -> String
displayPagination ti ip cp = let tp = totalPage ti ip
    in if tp >= 8
        then if ((tp - cp) > 3) && (cp > 4)
            then "<<Prev | ..."++(getString (cp-3) (cp+3) cp )++" | ... | Next>>"
            else if (cp <= 4)
                then "<<Prev "++(getString 1 7 cp )++" |...| Next>>"
            else "<<Prev | ..."++(getString (tp - 7) tp cp )++" | Next>>"
        else "<<Prev "++(getString 1 tp cp)++" | Next>>"

--follow up part
displayPagination2 :: NumOfPagesToDisplay -> TotalItems -> ItemsPerPage -> CurrentPage -> String
displayPagination2 np ti ip cp = let tp = if (ti `mod` ip == 0) then (ti `div` ip) else (ti `div` ip)+1
                                     nside = (np `div` 2) - 1
                                     ns = if (np `mod` 2) ==0 then np `div` 2 else (np `div` 2)+1
                                        in if tp > np
                                            then if ((tp - cp) > nside) && (cp > ns)
                                                then "<<Prev | ..."++(getString (cp-nside) (cp+nside) cp )++" | ... | Next>>"
                                                else if (cp <= ns)
                                                    then "<<Prev "++(getString 1 (np-1) cp )++" |...| Next>>"
                                                else "<<Prev | ..."++(getString (tp - (np-1)) tp cp )++" | Next>>"
                                            else "<<Prev "++(getString 1 tp cp)++" | Next>>"

main :: IO()
main = print (displayPagination2 9 100 4 10)