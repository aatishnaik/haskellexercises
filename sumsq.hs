sqr x = x * x
sumsq n = sqr (sum [1..n])

sqsumcal [] = 0
sqsumcal x = sqr (head x) + sqsumcal (tail x)
tolist x = [x]

sqsum a = sqsumcal [1..a]
main :: IO()
main = do
    print ((sumsq 100) - (sqsum 100))