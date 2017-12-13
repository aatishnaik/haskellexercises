
fibo :: Integer -> Integer
fibo 0 = 0
fibo 1 = 1
fibo n = fibo (n-1) + fibo (n-2)

fibonacci = map fibo [1..50]

main :: IO()
main = do
    print (fibonacci)
