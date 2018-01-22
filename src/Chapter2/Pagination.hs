module Chapter2.Pagination where
import Data.Char
import Data.List

totalPage :: TotalItems -> Int -> TotalPages
totalPage ti ip = if (ti `mod` ip == 0)
    then (ti `div` ip)
    else (ti `div` ip)+1

type ItemsPerPage = Int
type TotalItems = Int
type TotalPages = Int
type CurrentPage = Int
type FirstPage = Int
type LastPage = Int
type NumOfPagesToDisplay = Int

getString :: FirstPage -> LastPage -> CurrentPage -> String
getString fp lp cp = let
    first = intercalate "|" (map (\s -> show s) [fp..(cp-1)])
    current = "|" ++ (show cp) ++ "*|"
    second = intercalate "|" (map (\s -> show s) [(cp+1)..lp])
    in first ++ current ++ second

formatPagination :: FirstPage -> LastPage -> CurrentPage  -> (Bool,Bool) -> String
formatPagination fp lp cp (tpBool,cpBool) = 
        if (tpBool,cpBool)==(False,False)
            then "<<Prev "++(getString fp lp cp)++" Next>>"
        else if (tpBool,cpBool)==(False,True)
            then "<<Prev "++(getString fp lp cp)++"...Next>>"
        else if (tpBool,cpBool)==(True,False)
            then "<<Prev..."++(getString fp lp cp)++" Next>>"
        else if (tpBool,cpBool)==(True,True)
            then "<<Prev..."++(getString fp lp cp)++"...Next>>"
        else "INVALID STRING"

getPgLocation :: FirstPage -> LastPage -> Int -> CurrentPage  -> (Bool,Bool)
getPgLocation fp lp tp cp =
    if tp <= 8
        then (False,False)
    else (if fp == 1
                then (False,True)
            else if lp == tp
                then (True,False)
            else (True,True))

--part 1
displayPagination :: TotalItems -> ItemsPerPage -> CurrentPage -> String
displayPagination ti ip cp = let 
    tp = totalPage ti ip
    fp = if ((cp-4) < 1) then 1 else (cp-4)
    lp = if ((cp+4) > tp) then tp else (cp+4)
    cplocation = getPgLocation fp lp tp cp
    in formatPagination fp lp cp cplocation

--follow up part
displayPagination2 :: NumOfPagesToDisplay -> TotalItems -> ItemsPerPage -> CurrentPage -> String
displayPagination2 np ti ip cp = let tp = if (ti `mod` ip == 0) then (ti `div` ip) else (ti `div` ip)+1
                                     nside = (np `div` 2) - 1
                                     ns = if (np `mod` 2) ==0 then np `div` 2 else (np `div` 2)+1
                                        in if tp > np
                                            then if ((tp - cp) > nside) && (cp > ns)
                                                then "<<Prev | ..."++(getString (cp-nside) (cp+nside) cp )++" | ... | Next>>"
                                                else if (cp <= ns)
                                                    then "<<Prev |"++(getString 1 (np-1) cp )++" |...| Next>>"
                                                else "<<Prev | ...|"++(getString (tp - (np-1)) tp cp )++" | Next>>"
                                            else "<<Prev | "++(getString 1 tp cp)++" | Next>>"

main :: IO()
main = print (displayPagination 100 5 15)