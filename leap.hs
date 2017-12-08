--http://exercism.io/exercises/haskell/leap/readme
checkLeap yr = if (yr `mod` 4) == 0
    then if (yr `mod` 100) /= 0
            then putStrLn "Leap"
        else if (yr `mod` 400) /= 0
            then putStrLn "not Leap"
            else putStrLn "Leap"
        else putStrLn "not Leap"
main = do
    checkLeap (2015)