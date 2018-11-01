module Chapter1.Square where
square :: Int -> Int
square x = x*x

diff :: Int -> Int -> Int
diff x y = (square x) - (square y)
main :: IO()
main = do
    print (diff 3 6)