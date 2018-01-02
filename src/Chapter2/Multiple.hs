module Chapter2.Multiple where
import Data.List

--using filter
multiple num = filter (\x -> ((x `mod` 3) == 0) || ((x `mod` 5) == 0)) [1..num]

--using foldl'
summul n = foldl' (\ttl num -> if ((num `mod` 3) == 0) || ((num `mod` 5) == 0) then (ttl + num) else ttl) 0 [1..n]

main ::IO()
main = print (multiple 20)