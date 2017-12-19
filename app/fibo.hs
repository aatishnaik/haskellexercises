fibo :: Integer -> Integer
fibo 0 = 0
fibo 1 = 1
fibo n = fibo (n-1) + fibo (n-2)

fibonacci :: Integer -> [Integer]
fibonacci n = map fibo [1..n]

fibsum :: [Integer] -> Integer
fibsum [x] = if (x `mod` 2) == 0
    then x
    else 0
fibsum xs = if ((head xs) `mod` 2) == 0
    then head xs + fibsum (tail xs)
    else fibsum (tail xs)

--one func for task
efibsum :: Integer -> Integer -> Integer -> Integer
efibsum n1 n2 ttl
    |n2 < 4000000 = if (n2 `mod` 2) == 0
        then efibsum n2 (n1+n2) (ttl+n2)
        else efibsum n2 (n1+n2) ttl
    |n2 >= 4000000 = ttl
main :: IO()
main =
    print (efibsum 1 1 0)