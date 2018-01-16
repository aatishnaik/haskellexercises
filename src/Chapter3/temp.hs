module Chapter2.ExamScore2 where

import Data.List
import Data.Char

--data Customer = MCustomer
--    {   cid :: Int,
--        cname :: String,
--        csal :: Int
--    }
data ImperialWeight = MkImperialWeight Int Int Int Int deriving (Eq, Show, Ord)

addWeights :: ImperialWeight -> ImperialWeight -> ImperialWeight
addWeights (MkImperialWeight p1 o1 d1 g1) (MkImperialWeight p2 o2 d2 g2) =
  let (addDrachm, finalGrain) = divMod (g1 + g2) 7000
      (addOunce, finalDrachm) = divMod (d1 + d2 + addDrachm) 256
      (addPound, finalOunce) = divMod (o1 + o2 + addOunce) 16
  in (MkImperialWeight (p1 + p2 + addPound) finalOunce finalDrachm finalGrain)







  module Chapter3.ExamScore where
    import Data.Char
    import Data.List
    
    data Marksheet = MkMarksheet {
        studentName :: String,
        scoreList :: [(String,Int)]
    }
    type StudentName = String
    type SubjectName = String
    type SubjectMarks = Int
    type MarkSheet = [(String,[(String,Int)])]
    
    data ScoreList = MkScoreList {
        subjectName :: String,
        subjectMarks :: Int
    }
    
    subAvg MkMarksheet
    
    --subAvg :: [Marksheet] -> [String] -> String -> Float
    --subAvg [MkMarksheet {studenntName = sName,scoreList = sList}] subjects subname = 1.2
    {-}
    subAvg :: Marksheet -> [SubjectName] -> SubjectName -> Float
    subAvg markSheet subjects subjname =
        let validSubs = markSheet{-allValidNames -- subjects-}
            subs = map (\x -> (snd x)) validSubs
            subarr = map (\x -> (filter (\(namesub,markssub) -> namesub == subjname) x )) subs
            scoresarr = concat subarr
            scores = map (\(namesub,markssub) -> markssub) scoresarr
        in fromIntegral (sum scores) / fromIntegral (length scores)
    -}