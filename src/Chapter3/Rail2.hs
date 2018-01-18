module Chapter3.Rail2 where
import Data.Char
import Data.List
    
data Rail1 = MkRail1 String deriving Show
data Rail2 = MkRail2 String deriving Show
data Rail3 = MkRail3 String deriving Show

rail :: String -> Rail1 -> Rail2 -> Rail3 -> Int -> Int -> String
rail "" (MkRail1 rail1) (MkRail2 rail2) (MkRail3 rail3) _ _ = rail1 ++ rail2 ++ rail3
rail inputString (MkRail1 rail1) (MkRail2 rail2) (MkRail3 rail3) currentRail movementDirection
    |movementDirection == 0 = if currentRail == 1
        then rail (tail inputString) (MkRail1 (rail1 ++ [(head inputString)])) (MkRail2 rail2) (MkRail3 rail3) (currentRail+1) 0
        else if currentRail == 2
            then rail (tail inputString) (MkRail1 rail1) (MkRail2 (rail2 ++ [(head inputString)])) (MkRail3 rail3) (currentRail+1) 0
        else if currentRail == 3
            then rail (tail inputString) (MkRail1 rail1) (MkRail2 rail2) (MkRail3 (rail3 ++ [(head inputString)])) (currentRail-1) 1
        else rail "" (MkRail1 rail1) (MkRail2 rail2) (MkRail3 rail3) 0 0
    |movementDirection == 1 = if currentRail == 2
        then rail (tail inputString) (MkRail1 rail1) (MkRail2 (rail2 ++ [(head inputString)])) (MkRail3 rail3) (currentRail-1) 0
        else rail "" (MkRail1 rail1) (MkRail2 rail2) (MkRail3 rail3) 0 0

rmspace :: String -> String
rmspace str = if (length (delete ' ' str)) < (length str) 
            then rmspace (delete ' ' str)
            else delete ' ' str

encode :: String -> String
encode inputString = rail (rmspace inputString) (MkRail1 "") (MkRail2 "") (MkRail3 "") 1 0