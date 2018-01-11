module Chapter2.GenFibo where
import Data.Char
import Data.List

getarr :: (Int,Int,[Int]) -> [Int ]
getarr (n1,n2,arr) = arr

fibo :: Int -> (Int,Int,[Int])
fibo n = foldl' (\ (x,y,list) i -> (y,x+y,list ++ [x+y])) (0,1,[]) [1..(n*10)]

genFibo :: (Int -> Bool) -> Int -> [Int]
genFibo fn n =  (take n (filter (\num -> fn num) (getarr (fibo n))))

neven :: Int -> [Int]
neven n = genFibo (\x -> (x `mod` 2) == 0) n

ntri :: Int -> [Int]
ntri n = genFibo (\x -> (x `mod` 3) == 0) n

main :: IO()
main = print (neven 10)