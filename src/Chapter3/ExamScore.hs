module Chapter3.ExamScore where
import Data.Char
import Data.List

{-data Marksheet = MkMarksheet {
    studentName :: String,
    scoreList :: [(String,Int)]
}-}
type StudentName = String
type SubjectName = String
type SubjectMarks = Int
type MarkSheet = [(String,[(String,Int)])]

data ScoreList = MkScoreList {
    subjectName :: String,
    subjectMarks :: Int
}
type StudentName = String
type SubjectName = String
type SubjectMarks = Int
type MarkSheet = [(StudentName,[ScoreList])]
--subAvg :: [Marksheet] -> [String] -> String -> Float
--subAvg [MkMarksheet {studenntName = sName,scoreList = sList}] subjects subname = 1.2

--subAvg :: MarkSheet -> [SubjectName] -> SubjectName -> Float
subAvg markSheet subjects subjname =
    let validSubs = markSheet{-allValidNames-} -- subjects
        subs = map (\x -> (snd x)) validSubs
        subarr = map (\x -> (filter (\(namesub,markssub) -> namesub == subjname) x )) subs
        scoresarr = concat subarr
        scores = map (\(namesub,markssub) -> markssub) scoresarr
    in fromIntegral (sum scores) / fromIntegral (length scores)
