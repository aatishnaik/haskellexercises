module Chapter2.Nmultiple where
import Data.List

--nmultiples of 3 and 5

nmultiple :: Int -> [Int]
nmultiple n = take n (filter (\n->((n `mod` 3) == 0) || ((n `mod` 5) == 0)) [1..(n*3)])

--sum of multiples till n
summul :: Int -> Int
summul n = foldl' (\ttl num -> if ((num `mod` 3) == 0) || ((num `mod` 5) == 0) then (ttl + num) else ttl) 0 [1..n]
main :: IO()
main = print (nmultiple 10)