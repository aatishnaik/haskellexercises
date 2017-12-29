module Chapter1.Natural where
check 100 xs = []
check x xs = if ((x `mod` 3 ==0) || (x `mod` 5 ==0))
    then check x+1 (xs ++ (convert x))
    else check x+1 xs
convert :: x -> [x]
convert x = [x]