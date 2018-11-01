module Chapter1.Nmultiple where
checkmul :: Integer -> Bool
checkmul x = if ( x `mod` 3) == 0
    then True
    else if ( x `mod` 5) == 0
    then True
    else False

findmul :: Integer -> Integer -> Integer -> [Integer]
findmul num n s
        |n <= s = if (checkmul num)
            then ([num] ++ (findmul (num+1) (n+1) s))
            else (findmul (num+1) n s)
        |otherwise = []

nmultiple :: Integer -> Integer
nmultiple n = sum (findmul 1 1 n)