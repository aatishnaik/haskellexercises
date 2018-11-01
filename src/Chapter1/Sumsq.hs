module Chapter1.Sumsq where
sumsq :: Int -> Int -> Int -> Int
sumsq n sum s
    | n <= s = sumsq (n+1) (sum+n) s
    | otherwise = sum*sum
sqsum :: Int -> Int -> Int -> Int
sqsum n sum s
    | n <= s = sqsum (n+1) (sum + (n * n)) s
    | otherwise = sum

main :: IO()
main = do
    print ((sumsq 0 0 1000) - (sqsum 0 0 1000))