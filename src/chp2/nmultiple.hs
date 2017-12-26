import Data.List
--check :: Int -> Int -> Int -> Int
check (num,x,n) = if ((num `mod` 3) == 0) || ((num `mod` 5) == 0)
    then if x < n
        then check num+1 x+1 n
        else num
    else check (num+1,x,n)

nmultiple n = map check [1..n]
nmultiple n = foldl' (\num a -> check (num,0,a)) 1 [1..n]