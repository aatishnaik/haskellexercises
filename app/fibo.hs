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
efibsum n ttl
    |n > 0 = if ((fibo n) `mod` 2) == 0
        then ttl + (efibsum (n-1) (ttl+(fibo n)))
        else (efibsum (n-1) ttl)
    |n <= 0 = 0

main :: IO()
main =
    print (efibsum 20 0)