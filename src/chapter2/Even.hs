module Chapter2.Even where
import Data.List
check num = if (num `mod` 2) == 0
    then True
    else False
even n = filter check [1..n]

even2 n = foldl' (\evenList x -> if ((x `mod` 2) == 0) then (evenList ++ [x]) else (evenList ++ [])) [] [1..n]