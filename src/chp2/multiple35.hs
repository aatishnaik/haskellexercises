import Data.List
check num = if ((num `mod` 3) == 0) || ((num `mod` 5) == 0)
    then True
    else False
multiple list = filter check list