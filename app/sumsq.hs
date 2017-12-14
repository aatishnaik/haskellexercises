sqr :: Int -> Int
sqr x = x * x
sumsq :: Int -> Int
sumsq n = sqr (sum [1..n])

sqsumcal :: [Int] -> Int
sqsumcal [] = 0
sqsumcal x = sqr (head x) + sqsumcal (tail x)

sqsum :: Int-> Int
sqsum a = sqsumcal [1..a]
main :: IO()
main = do
    print ((sumsq 100) - (sqsum 100))