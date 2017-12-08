calc :: Float -> Float
calc x = ((x/3600)/24)/365
main :: IO()
main = do
    print (calc 1000000000)