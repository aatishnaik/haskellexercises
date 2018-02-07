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

decodeLength :: Expr -> String -> String
decodeLength e arr = case e of 
    EChar c v _ prev -> decodeLength prev ((printChar v c)++ arr)
    Last -> arr
printChar :: Char -> Int -> String
printChar c n = foldl' (\str _ -> str++[c]) "" [1..n]


{--with nmbers string to string will create a problem as we wont know if a particular number 
is a char or a frequency. We also cannot consider the last digit as char and the digits prior 
to it as frequency as the input is one single string and not a list of strings. Hence to 
overcome this problem we save it in a structure that maintains the information needed in 
order for us to decode the text.--}