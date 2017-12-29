module Chapter1.Rail3 where
import Data.Char
import Data.List

--rmchar :: String -> Int -> String
--rmchar str n max = if n < max
--    then rmchar (tail str) (n+1)
--    else tail str

rail :: String -> String -> String -> String -> Int -> Int -> String
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
