module Chapter2.Even where
import Data.List
check num = if (num `mod` 2) == 0
    then True
    else False
even n = filter check [1..n]