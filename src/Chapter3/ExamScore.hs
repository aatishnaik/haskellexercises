module Chapter3.ExamScore2 where
import Data.Char
import Data.List
        
data StudentName = MkStudentName String deriving Show
data SubjectName = MkSSubjectName String deriving Show
data SubjectMarks = MkSubjectMarks Int deriving Show
data MarkSheet = [(StudentName,[(SubjectName,SubjectMarks)])]
    
--subAvg :: MarkSheet -> [SubjectName] -> SubjectName -> Float
subAvg mksheet subjects subname =
    let [(sname,score)] = mksheet
    in sname