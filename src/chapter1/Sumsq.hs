module Chapter1.Sumsq where
sumsq :: Int -> Int -> Int
sumsq n sum
    | n <= 1000 = sumsq (n+1) (sum+n)
    | otherwise = sum*sum
sqsum :: Int -> Int -> Int
sqsum n sum
    | n <= 1000 = sqsum (n+1) (sum + (n * n))
    | otherwise = sum

main :: IO()
main = do
    print ((sumsq 0 0) - (sqsum 0 0))