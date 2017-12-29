module Chapter2.Fibo where
import Data.Char
import Data.List

fibo :: Integer -> Integer
fibo 0 = 0
fibo 1 = 1
fibo n = fibo (n-1) + fibo (n-2)

efibsum :: Integer -> Integer
efibsum n = foldl' (\ttl x -> if ((x `mod` 2) == 0) then (ttl + x) else ttl) 0 (map fibo [1..n])