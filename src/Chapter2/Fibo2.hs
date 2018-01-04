import Data.List
fibo :: Int -> (Int,Int,[Int])
fibo n = foldl' (\ (x,y,list) i -> (y,x+y,list ++ [x+y])) (0,1,[]) [1..n]

evenf :: Int -> (Int,Int,[Int]) -> Int
evenf n (x,y,z) = sum (filter (\c -> c `mod` 2 == 0) z)

evenfib :: Int -> Int
evenfib n = evenf n (fibo n)

main :: IO()
main = print(evenfib 10)