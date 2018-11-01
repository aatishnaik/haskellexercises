--http://exercism.io/exercises/haskell/space-age/readme
module Chapter1.Space where
calc :: Float -> Float
calc x = ((x/3600)/24)/365
main :: IO()
main = do
    print (calc 1000000000)