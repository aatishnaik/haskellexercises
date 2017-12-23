checkmul :: Integer -> Integer
checkmul x = if ( x `mod` 3) == 0
    then x
    else if ( x `mod` 5) == 0
    then x
    else 0
findmul :: Integer -> Integer -> Integer -> [Integer]
findmul num n s
        |n <= s = if (checkmul num) == num
            then ([num] ++ (findmul (num+1) (n+1) s))
            else ([] ++ findmul (num+1) n s)
        |otherwise = []
nmultiple :: Integer -> [Integer]
nmultiple n = findmul 1 1 n