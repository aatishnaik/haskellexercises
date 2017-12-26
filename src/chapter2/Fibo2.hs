import Data.List
new = foldl' (\ (x,y,fiboList) i -> (y,x+y,fiboList ++ [x+y])) (0,1,[]) [1,2,3,4,5,6,7,8,9,10]