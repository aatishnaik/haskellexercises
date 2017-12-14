--http://exercism.io/exercises/haskell/triangle/readme
data Triangle = Triangle { s1 :: Int
                            , s2 :: Int
                            , s3 :: Int
                            } deriving (Show)
equi :: Integer -> Integer -> Integer -> IO()
equi a b c
    | (a == b) && (b == c) = putStrLn "Equi"
    | (a /= b) && (b == c ) || (a == b) && (b /= c ) || (a == c) && (b /= a )= putStrLn "Iso"
    | ((a /= b) && (b /= c )) && (a /= c )= putStrLn "Scalene"

main = do
    equi 2 1 3

   
