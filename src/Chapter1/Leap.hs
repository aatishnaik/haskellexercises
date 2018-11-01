--http://exercism.io/exercises/haskell/leap/readme
module Chapter1.Leap where
checkLeap :: Int -> Bool
checkLeap yr = if (yr `mod` 4) == 0
    then if (yr `mod` 100) /= 0
            then True
        else if (yr `mod` 400) /= 0
            then False
            else True
        else False
main :: IO()
main = do
    print (checkLeap (2015))
