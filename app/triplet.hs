checklarge :: Int -> Int -> Int -> Bool
checklarge x y z = if (x > y) && (x > z)
    then checktrip x y z
    else if (y > x) && (y > z)
    then checktrip y x z
    else if (z > x) && (z > y)
    then checktrip z y x
    else False
checktrip :: Int -> Int -> Int -> Bool
checktrip x y z = ((x*x) == ((y*y)+(z*z)))
main :: IO()
main = do
    print (checktrip 2 4 5)
