module Chapter2.Nmultiple where
import Data.List

checkmul :: Integer -> Integer
checkmul x = if ( x `mod` 3) == 0
    then x
    else if ( x `mod` 5) == 0
    then x
    else 0

check :: (Int,Int,Int) -> Int
check (x,n,num) = if ((num `mod` 3) == 0) || ((num `mod` 5) == 0)
    then if x < n
        then check (x+1,n,num+1)
        else num
    else check (x,n,num+1)
nmultiple n = map (\x -> check (0,x,0)) [1..n]