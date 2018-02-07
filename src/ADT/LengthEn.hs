module ADT.LengthEncoding where
import Data.List

data Expr = EChar Int Char Expr Expr
    | Last
    deriving (Eq, Show, Ord)

lengthEncode :: String -> String
lengthEncode str = 
    let node = encodeLength Last str
    in "Encoded: "++(showlist node "") ++ "  Decoded: " ++decodeLength node ""

encodeLength :: Expr -> String -> Expr
encodeLength e inputStr = 
    let inp = group inputStr
        (count,ch) = if (length inp) > 0 then (length (head inp),head (head inp)) else (0,'x')
        remString = concat (tail inp)
    in case e of
        EChar _ _ nxt _-> 
            if count == 0
                then e
                else encodeLength (EChar count ch nxt e) remString
        Last -> encodeLength (append e (EChar count ch Last e)) remString

append :: Expr -> Expr -> Expr
append e value = case e of 
    EChar num val nxt _ -> EChar num val (append nxt value) e
    Last -> 
        let EChar n v _ _= value
        in EChar n v Last e

showlist :: Expr -> String -> String
showlist e arr= case e of 
    EChar c v _ prev -> 
        let str = if c > 1 then (show c)++[v]++arr else [v]++arr
        in showlist prev str
    Last -> arr

decodeLength e arr = case e of 
    EChar c v _ prev -> decodeLength prev ((printChar v c)++ arr)
    Last -> arr

printChar c n = foldl' (\str _ -> str++[c]) "" [1..n]