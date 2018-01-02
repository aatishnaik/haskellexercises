import Data.List
evenfib :: Int -> Int
evenfib n = let ch = changetype (fibo n)
    in sum (filter (\c -> c `mod` 2 == 0) ch)

changetype :: (Int,Int,[Int]) -> [Int]
changetype (x,y,z) = z

fibo :: Int -> (Int,Int,[Int])
fibo n = foldl' (\ (x,y,list) i -> (y,x+y,list ++ [x+y])) (0,1,[]) [1..n]

main :: IO()
main = print(evenfib 10)