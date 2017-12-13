aaa x y = let r = 3 
              s = 6
              in  r*x + s*y
              
data Roygbiv = Red
             | Orange
             | Yellow
             | Green
             | Blue
             | Indigo
             | Violet
               deriving (Eq, Show)

max3 :: (Double, Double, Double) -> Double
max3 (x, y, z) = max (max x y) z

data Customer = Customer Int String [String]
                deriving (Show)

customerID :: Customer -> Int
customerID (Customer id _ _) = id
customerName :: Customer -> String
customerName (Customer _ name _) = name
customerAddress :: Customer -> [String]
customerAddress (Customer _ _ address) = address

data List a = Cons a (List a)
            | Nil
              deriving (Show)