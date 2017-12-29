module Chapter2.Multiple where
import Data.List
check num = if ((num `mod` 3) == 0) || ((num `mod` 5) == 0)
    then True
    else False
multiple list = filter check list

--using foldl'
summul n = foldl' (\ttl num -> if ((num `mod` 3) == 0) || ((num `mod` 5) == 0) then (ttl + num) else ttl) 0 [1..n]