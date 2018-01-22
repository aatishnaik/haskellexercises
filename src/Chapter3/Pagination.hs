module Chapter3.Pagination where
import Data.Char
import Data.List

data Page = MkPage Int deriving (Eq, Show, Ord)
data NItems = MkNItems Int deriving (Eq, Show, Ord)
data NPage = MkNPage Int deriving (Eq, Show, Ord)

totalPage :: NItems -> NItems -> Int
totalPage (MkNItems ti) (MkNItems ip) = if ((ti `mod` ip) == 0)
    then (ti `div` ip)
    else (ti `div` ip)+1

getString :: Page -> Page -> Page -> String
getString (MkPage fp) (MkPage lp) (MkPage cp) = let
    first = intercalate "|" (map (\s -> show s) [fp..(cp-1)])
    current = "|" ++ (show cp) ++ "*|"
    second = intercalate "|" (map (\s -> show s) [(cp+1)..lp])
    in first ++ current ++ second

formatPagination :: Page -> Page -> Page -> (Bool,Bool) -> String
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

getPgLocation :: Page -> Page -> NPage -> Page  -> (Bool,Bool)
getPgLocation (MkPage fp) (MkPage lp) (MkNPage tp) cp =
    if tp <= 8
        then (False,False)
    else (if fp == 1
                then (False,True)
            else if lp == tp
                then (True,False)
            else (True,True))

displayPagination :: NItems -> NItems -> Page -> String
displayPagination ti ip (MkPage cp) = let 
    tp = totalPage ti ip
    fp = if ((cp-4) < 1) then 1 else (cp-4)
    lp = if ((cp+4) > tp) then tp else (cp+4)
    cplocation = getPgLocation (MkPage fp) (MkPage lp) (MkNPage tp) (MkPage cp)
    in formatPagination (MkPage fp) (MkPage lp) (MkPage cp) cplocation