module Chapter3.Rail1 where
import Data.Char
import Data.List
--using types

type Rail1 = String
type Rail2 = String
type Rail3 = String

rail :: String -> Rail1 -> Rail2 -> Rail3 -> Int -> Int -> String
rail "" rail1 rail2 rail3 _ _ = rail1 ++ rail2 ++ rail3
rail inputString rail1 rail2 rail3 currentRail movementDirection
    |movementDirection == 0 = if currentRail == 1
        then rail (tail inputString) (rail1 ++ [(head inputString)]) rail2 rail3 (currentRail+1) 0
        else if currentRail == 2
            then rail (tail inputString) rail1 (rail2 ++ [(head inputString)]) rail3 (currentRail+1) 0
        else if currentRail == 3
            then rail (tail inputString) rail1 rail2 (rail3 ++ [(head inputString)]) (currentRail-1) 1
        else rail "" rail1 rail2 rail3 0 0
    |movementDirection == 1 = if currentRail == 2
        then rail (tail inputString) rail1 (rail2 ++ [(head inputString)]) rail3 (currentRail-1) 0
        else rail "" rail1 rail2 rail3 0 0
encode :: String -> String
encode inputString = rail inputString "" "" "" 1 0

main :: IO()
main =
    print (encode "abcdefghijklmn")
