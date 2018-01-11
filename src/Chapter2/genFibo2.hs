module Chapter2.GenFibo2 where
import Data.Char
import Data.List

fibo :: Int -> Int
fibo 0 = 0
fibo 1 = 1
fibo n = fibo (n-1) + fibo (n-2)
narr :: Int -> Int -> [Int]
narr num n
    |num < n = [fibo num] ++ (narr (num+1) n)
    |num == n = [fibo num]

genFibo :: (Int -> Bool) -> Int -> [Int]
genFibo fn n =  (take n (filter (\num -> fn num) (narr 1 (n*10))))

neven :: Int -> [Int]
neven n = genFibo (\x -> (x `mod` 2) == 0) n

ntri :: Int -> [Int]
ntri n = genFibo (\x -> (x `mod` 3) == 0) n

main :: IO()
main = print (neven 10)