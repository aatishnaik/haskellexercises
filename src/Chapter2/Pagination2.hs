module Chapter2.Pagination2 where
import Data.Char
import Data.List
getString :: Int -> Int -> Int -> String
getString fp lp cp = (foldl' (\ str x -> str ++" | "++ (show x)) "" [fp..cp]) ++ (foldl' (\ str x -> str ++" | "++ (show x)) "*" [(cp+1)..lp])
    
type ItemsPerPage = Int
type TotalItems = Int
type CurrentPage = Int
type NumOfPagesToDisplay = Int
displayPagination :: NumOfPagesToDisplay -> TotalItems -> ItemsPerPage -> CurrentPage -> String
displayPagination np ti ip cp = let tp = if (ti `mod` ip == 0) then (ti `div` ip) else (ti `div` ip)+1
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
main = print (displayPagination 9 100 3 4)