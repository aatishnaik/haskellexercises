module Chapter2.Nmultiple where
import Data.List

check :: (Int,Int,Int) -> Int
check (x,n,num) = if ((num `mod` 3) == 0) || ((num `mod` 5) == 0)
    then if x < n
        then check (x+1,n,num+1)
        else num
    else check (x,n,num+1)
nmultiple :: Int -> [Int]
nmultiple n = map (\x -> check (0,x,0)) [1..n]
summul :: Int -> Int
summul n = foldl' (\ttl num -> if ((num `mod` 3) == 0) || ((num `mod` 5) == 0) then (ttl + num) else ttl) 0 [1..n]
