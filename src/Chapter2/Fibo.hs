module Chapter2.Fibo where
import Data.Char
import Data.List

fibo :: Int -> (Int,Int,[Int])
fibo n = foldl' (\ (x,y,list) i -> (y,x+y,list ++ [x+y])) (0,1,[]) [1..n]

main :: IO()
main = print (fibo 10)