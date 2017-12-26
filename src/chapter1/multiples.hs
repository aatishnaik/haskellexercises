checkmul :: Integer -> Integer
checkmul x = if ( x `mod` 3) == 0
    then x
    else if ( x `mod` 5) == 0
    then x
    else 0

tolist x = [x]

findmul :: Integer -> Integer
--findmul xs = (tolist (checkmul (head xs))) ++ (findmul (tail xs))
findmul n 
        |n < 10 = if (checkmul n) == n
            then (n + (findmul (n+1)))
            else findmul (n+1)
        |otherwise = 0

main :: IO()
main = print (findmul 0)
--    print (sum (findmul [1..1000-1]))