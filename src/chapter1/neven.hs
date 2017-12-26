evenList :: Int -> [Int] -> [Int]
evenList n [] = []
evenList 0 list = []
evenList n list = [(head list) * 2] ++ evenList (n-1) (tail list)

neven n = evenList n [1..n]