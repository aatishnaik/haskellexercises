module Chapter2.Even where
import Data.List

even :: Int -> [Int]
even n = filter (\ num -> (num `mod` 2) == 0) [1..n]
even2 :: Int -> [Int]
even2 n = foldl' (\evenList x -> if ((x `mod` 2) == 0) then (evenList ++ [x]) else (evenList)) [] [1..n]
main ::IO()
main = print (Chapter2.Even.even2 10)