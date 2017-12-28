import Data.List
fibo n = foldl' (\ (x,y,fiboList) i -> (y,x+y,fiboList ++ [x+y])) (0,1,[]) [1..n]