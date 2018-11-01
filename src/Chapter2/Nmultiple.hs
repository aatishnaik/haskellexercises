module Chapter2.Nmultiple where
import Data.List

--nmultiples of 3 and 5

nMultiple :: Int -> [Int]
nMultiple n = take n (filter (\n->((n `mod` 3) == 0) || ((n `mod` 5) == 0)) [1..(n*3)])

--sum of multiples till n
sumMul :: Int -> Int
sumMul n = foldl' (\ttl num -> if ((num `mod` 3) == 0) || ((num `mod` 5) == 0) then (ttl + num) else ttl) 0 [1..n]
main :: IO()
main = print (nMultiple 10)