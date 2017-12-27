module Chapter2.SumSq where
import Data.Char
import Data.List

sumsq :: Int -> Int
--sumsq n = sum (map (\c -> (c*c)) [1..n])
sumsq n = (foldl' (\ttl x -> ttl + (x ^ 2)) 0 [1..n])

sqsum :: Int -> Int
sqsum n = (foldl' (\ttl x -> ttl + x) 0 [1..n])^2

diff :: Int
diff = (sqsum 1000) - (sumsq 1000)