module Chapter2.Pagination where
import Data.List

totalInt :: Int -> Int -> Int
totalInt ti ip = if ((ti `mod` ip) == 0)
    then (ti `div` ip)
    else (ti `div` ip)+1

getString :: Int -> Int -> Int -> String
getString fp lp cp = intercalate " | " (map (\s -> (show s) ++ (if s==cp then "*" else "")) [fp..lp])

formatPagination :: Int -> Int -> Int -> (Bool,Bool) -> String
formatPagination fp lp cp (tpBool,cpBool) = case (tpBool,cpBool) of
    (False,False) -> "<<Prev "++(getString fp lp cp)++" Next>>"
    (False,True) -> "<<Prev "++(getString fp lp cp)++"...Next>>"
    (True,False) -> "<<Prev..."++(getString fp lp cp)++" Next>>"
    (True,True) -> "<<Prev..."++(getString fp lp cp)++"...Next>>"

getPgLocation :: Int -> Int -> Int -> Int  -> (Bool,Bool)
getPgLocation fp lp tp _ =
    if tp <= 8
        then (False,False)
    else (if fp == 1
                then (False,True)
            else if lp == tp
                then (True,False)
            else (True,True))

displayPagination :: Int -> Int -> Int -> String
displayPagination ti ip cp = let 
    tp = totalInt ti ip
    fp = if ((cp-4) < 1) then 1 else (cp-4)
    lp = if ((cp+4) > tp) then tp else (cp+4)
    cplocation = getPgLocation fp lp tp cp
    in formatPagination fp lp cp cplocation