module Chapter3.ExamScore2 where
import Data.Char
import Data.List
        
data StudentName = MkStudentName String deriving Show
data SubjectName = MkSubjectName String deriving Show
data SubjectMarks = MkSubjectMarks Int deriving Show
data MarkSheet = MkMarkSheet [(StudentName,[(SubjectName,SubjectMarks)])] deriving Show

--subAvg :: MarkSheet -> [SubjectName] -> SubjectName -> Float

subAvg (MkMarkSheet [(MkStudentName studname,[(MkSubjectName subjname,MkSubjectMarks subjmarks)])]) (MkSubjectName subname) =
    let sname = map (\(s,scr) -> s) [(studname,[(subjname,subjmarks)])]
    in sname