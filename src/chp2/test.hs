add = let n1 = 10
        n2 = 20
        n3 = 30
    in n1 + n2 + n3

sqr = let square x = x * x in (square 5, square 3, square 2)
cylinder r h = let sideArea = 2 * pi * r * h  
        topArea = pi * r ^2  
    in  sideArea + 2 * topArea  