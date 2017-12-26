module Chapter2.SumEven where
import Data.List

sumeven list = foldl' (\ttl x -> if ((x `mod` 2) == 0) then (ttl + x) else ttl) 0 list