checkmul x = if ( x `mod` 3) == 0
    then x
    else if ( x `mod` 5) == 0
    then x
    else 0

tolist x = [x]
findmul [] = [0]
findmul xs = (tolist (checkmul (head xs))) ++ (findmul (tail xs))
main :: IO()
main = do
    print (sum (findmul [1..1000-1]))