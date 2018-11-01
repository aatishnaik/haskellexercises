module Chapter2.SumInt where
import Data.List

sumInt list = foldl' (\ttl x -> ttl + x) 0 list