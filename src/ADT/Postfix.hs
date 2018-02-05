module ADT.Postfix where

data Node = Element Float Node
    | Empty
    deriving (Eq, Show, Ord)

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

calculate :: Expr -> Node -> (Bool,String,Float)
calculate e stck = case e of
    ENumb n ex -> calculate ex (push stck n)
    EAdd ex -> 
        let Element n1 s1 = pop stck
            Element n2 s2 = pop s1
        in calculate ex (push s2 (n1 + n2))
    ESub ex -> 
        let Element n1 s1 = pop stck
            Element n2 s2 = pop s1
        in calculate ex (push s2 (n1 - n2))
    EMul ex -> 
        let Element n1 s1 = pop stck
            Element n2 s2 = pop s1
        in calculate ex (push s2 (n1 * n2))
    EDiv ex -> 
        let Element n1 s1 = pop stck
            Element n2 s2 = pop s1
        in if n1 == 0
            then (False,"Divide by zero",0.0)
            else calculate ex (push s2 (n1 / n2))
    ENone -> 
        let flag = (getLength stck 0) == 1
            Element ans _ = pop stck
        in if flag
            then (True,"Valid expression",ans)
        else (False,"Invalid expression",0.0)

pop :: Node -> Node
pop first = case first of 
    Element _ _ -> first
    Empty -> Empty

push :: Node -> Float -> Node
push first value = case first of 
    Element _ _ -> Element value first
    Empty -> Element value Empty

getLength :: Node -> Int -> Int
getLength first cpos = case first of 
    Element _ nxt -> getLength nxt (cpos+1)
    Empty -> cpos