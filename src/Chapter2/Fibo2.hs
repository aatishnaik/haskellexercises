import Data.List

evenfib n = sum (filter (\c -> c `mod` 2 == 0) (changetype (fibo n)))

changetype :: (Int,Int,[Int]) -> [Int]
changetype (x,y,z) = z

fibo :: Int -> (Int,Int,[Int])
fibo n = foldl' (\ (x,y,list) i -> (y,x+y,list ++ [x+y])) (0,1,[]) [1..n]

main :: IO()
main = print(evenfib 10)