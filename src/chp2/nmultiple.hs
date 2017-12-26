import Data.List

checkmul :: Integer -> Integer
checkmul x = if ( x `mod` 3) == 0
    then x
    else if ( x `mod` 5) == 0
    then x
    else 0

--check :: Int -> Int -> Int -> Int
check (x,n,num) = if ((num `mod` 3) == 0) || ((num `mod` 5) == 0)
    then if x < n
        then check num+1 x+1 n
        else num
    else check (x,n,num+1)

nmultiple n = map (check (0,5,n) [1..n]
--nmultiple n = foldl' (\num a -> check (num,0,a)) 1 [1..n]