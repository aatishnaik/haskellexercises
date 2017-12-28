module Chapter2.SumInt where
import Data.List

sumint list = foldl' (\ttl x -> ttl + x) 0 list

--using filter
--sumint2 list = filter