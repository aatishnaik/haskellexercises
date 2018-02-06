module ADT.NewPostfix where

data Node = 
    Element Expr Node 
    | Empty
data Expr = ENumb Float 
    | EMul 
    | EAdd 
    | ESub 
    | EDiv

expr1 :: Node
expr1 = Element (ENumb 10.0) (Element (ENumb 20.0) (Element EAdd Empty))
expr2 :: Node
expr2 = Element (ENumb 10.0) (Element (ENumb 20.0) (Element (ENumb 30.0) (Element EAdd Empty)))
expr3 :: Node
expr3 = Element (ENumb 10.0) (Element (ENumb 20.0) (Element (ENumb 20.0) (Element ESub (Element EDiv Empty))))

calculate :: Node -> Node -> (Bool,String,Float)
calculate e stck = case e of
    Element expEle nxt -> 
        case expEle of
            ENumb n -> calculate nxt (push stck (ENumb n))
            EAdd -> 
                let Element n1 s1 = pop stck
                    Element n2 s2 = pop s1
                    ENumb x1 = n1
                    ENumb x2 = n2
                in calculate nxt (push s2 (ENumb (x1 + x2)))
            ESub -> 
                let Element n1 s1 = pop stck
                    Element n2 s2 = pop s1
                    ENumb x1 = n1
                    ENumb x2 = n2
                in calculate nxt (push s2 (ENumb (x1 - x2)))
            EMul -> 
                let Element n1 s1 = pop stck
                    Element n2 s2 = pop s1
                    ENumb x1 = n1
                    ENumb x2 = n2
                in calculate nxt (push s2 (ENumb (x1 * x2)))
            EDiv -> 
                let Element n1 s1 = pop stck
                    Element n2 s2 = pop s1
                    ENumb x1 = n1
                    ENumb x2 = n2
                in if x1 == 0
                    then (False,"Divide by zero",0.0)
                    else calculate nxt (push s2 (ENumb (x1 / x2)))
    Empty -> 
        let flag = (getLength stck 0) == 1
            Element n _ = pop stck
            ENumb ans = n
        in if flag
            then (True,"Valid expression",ans)
        else (False,"Invalid expression",0.0)

pop :: Node -> Node
pop first = case first of 
    Element _ _ -> first
    Empty -> Empty

push :: Node -> Expr -> Node
push first value = case first of 
    Element _ _ -> Element value first
    Empty -> Element value Empty

getLength :: Node -> Int -> Int
getLength first cpos = case first of 
    Element _ nxt -> getLength nxt (cpos+1)
    Empty -> cpos