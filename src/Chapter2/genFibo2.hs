module Chapter2.GenFibo2 where
import Data.Char
import Data.List

fibo :: Int -> Int
fibo 0 = 0
fibo 1 = 1
fibo n = fibo (n-1) + fibo (n-2)
narr num n
    |num < n = [fibo num] ++ (narr (num+1) n)
    |num == n = [fibo num]

neven2 num n arr
    |num <= n = if ((head arr) `mod` 2) == 0
                then [head arr] ++ (neven2 (num+1) n (tail arr))
                else neven2 (num) n (tail arr)
    |num > n = []

neven n = neven2 1 n (narr 1 (n*10))
main :: IO()
main = print (neven 10)