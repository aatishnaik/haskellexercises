--http://exercism.io/exercises/haskell/collatz-conjecture/readme
module Chapter1.Collaz where
collaz :: Int -> Int
collaz x = if (x `mod` 2) == 0
    then x `div` 2
    else (x * 3)+1
applycollaz x=apply x 0
apply x y
    | x==1 = y
    | x/=1 = apply (collaz x) y+1
