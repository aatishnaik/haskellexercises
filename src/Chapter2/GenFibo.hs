module Chapter2.GenFibo where
import Data.Char
import Data.List

genFibo :: Int -> Int -> [Int]
genFibo = \x -> \n -> 
    if x == 2
        then neven n
    else if x == 3
        then ntri n
    else []

getarr :: (Int,Int,[Int]) -> [Int ]
getarr (n1,n2,arr) = arr

fibo :: Int -> (Int,Int,[Int])
fibo n = foldl' (\ (x,y,list) i -> (y,x+y,list ++ [x+y])) (0,1,[]) [1..(n*10)]

neven :: Int -> [Int]
neven n = take n (filter (\n->((n `mod` 2) == 0)) (getarr (fibo n)))

ntri :: Int -> [Int]
ntri n = take n (filter (\n->((n `mod` 3) == 0)) (getarr (fibo n)))

main :: IO()
main = print (genFibo 2 10)