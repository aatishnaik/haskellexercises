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

efibsum x y ttl
    |y < 13 = if ((x+y) `mod` 2) == 0
        then ttl + (efibsum y (x+y) (ttl+x+y))
        else (efibsum y (x+y) ttl)
    |y >= 13 = 0

main :: IO()
main =
    print (fibsum (fibonacci 8))