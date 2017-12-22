import Data.List
import Data.Char

stringcheck str str2 = foldl' (\ttl x -> if ((x `mod` 2) == 0) then (ttl + x) else ttl) 0 list