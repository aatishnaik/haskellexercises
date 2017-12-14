
fibo :: Integer -> Integer
fibo 0 = 0
fibo 1 = 1
fibo n = fibo (n-1) + fibo (n-2)

fibonacci = map fibo [1..20]
fibsum [x] = x
fibsum xs = if ((head xs) `mod` 2) == 0
    then (head xs) + (fibsum (tail xs))
    else fibsum (tail xs)
main :: IO()
main = do
    print (fibsum (fibonacci))