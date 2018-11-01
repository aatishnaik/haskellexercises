module ADT.OldPostfix where

data Expr = ENumb Float Expr
          | EMul Expr
          | EAdd Expr
          | ESub Expr
          | EDiv Expr
          | ENone
          deriving (Eq, Show, Ord)

expr1 :: Expr
expr1 = ENumb 10.0 (ENumb 20.0 (EAdd ENone))
expr2 :: Expr
expr2 = ENumb 10.0 (ENumb 20.0 (ENumb 30.0 (EAdd ENone)))
expr3 :: Expr
expr3 = ENumb 10.0 (ENumb 20.0 (ENumb 20.0 (ESub (EDiv ENone))))

calculate :: Expr -> [Float] -> (Bool,String,Float)
calculate e stck = case e of
    ENumb n ex -> calculate ex ([n] ++ stck)
    EAdd ex -> 
        let (n1,s1) = (head stck,tail stck)
            (n2,s2) = (head s1,tail s1)
        in calculate ex ([(n1 + n2)] ++ s2)
    ESub ex -> 
        let 
            (n1,s1) = (head stck,tail stck)
            (n2,s2) = (head s1,tail s1)
        in calculate ex ([(n1 - n2)] ++ s2)
    EMul ex -> 
        let 
            (n1,s1) = (head stck,tail stck)
            (n2,s2) = (head s1,tail s1)
        in calculate ex ([(n1 * n2)] ++ s2)
    EDiv ex -> 
        let 
            (n1,s1) = (head stck,tail stck)
            (n2,s2) = (head s1,tail s1)
        in if n1 == 0
            then (False,"Divide by zero",n1 / n2)
            else calculate ex ([(n1 / n2)] ++ s2)
    ENone -> 
        let 
            flag = (length stck) == 1
        in if flag
            then (True,"Valid expression",head stck)
        else (False,"Invalid expression",0.0)