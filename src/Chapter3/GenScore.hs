{-# LANGUAGE ScopedTypeVariables #-}
module Chapter3.GenExamScore where
import Data.List

data StudentName = MkStudentName String deriving (Eq, Show, Ord)
data SubjectName = MkSubjectName String deriving (Eq, Show, Ord)
data SubjectMarks = MkSubjectMarks Int deriving (Eq, Show, Ord)
type MarkSheet = [(StudentName,[(SubjectName,SubjectMarks)])]

mksheet :: MarkSheet
mksheet = [(MkStudentName "Saurabh Nanda",[(MkSubjectName "English",MkSubjectMarks 84), (MkSubjectName "Chemistry",MkSubjectMarks 80), (MkSubjectName "Physics",MkSubjectMarks 95), (MkSubjectName "Geography",MkSubjectMarks 75)]),(MkStudentName "John Doe", [(MkSubjectName "Chemistry",MkSubjectMarks 80), (MkSubjectName "Physics",MkSubjectMarks 95), (MkSubjectName "Geography",MkSubjectMarks 75)]),(MkStudentName "Jane Doe", [(MkSubjectName "Chemistry",MkSubjectMarks 66), (MkSubjectName "Phsyics",MkSubjectMarks 33), (MkSubjectName "Geography",MkSubjectMarks 56)]), (MkStudentName "John Doe", [(MkSubjectName "Chemistry",MkSubjectMarks 90), (MkSubjectName "Economics",MkSubjectMarks 45), (MkSubjectName "Geography",MkSubjectMarks 56)]),(MkStudentName "Bahubali", [(MkSubjectName "Hindi",MkSubjectMarks 45), (MkSubjectName "Biology",MkSubjectMarks (-90)), (MkSubjectName "Geography",MkSubjectMarks (-75))]),(MkStudentName "Rajnikant", [(MkSubjectName "Tamil",MkSubjectMarks 110), (MkSubjectName "Biology",MkSubjectMarks 100), (MkSubjectName "Geography",MkSubjectMarks 100)])]
subjects :: [SubjectName]
subjects = [MkSubjectName "English",MkSubjectName "Geography",MkSubjectName "Physics",MkSubjectName "Chemistry",MkSubjectName "Economics",MkSubjectName "Computer Science"]

--Part 1

foldScore :: (b -> (StudentName,Bool,[(SubjectName,SubjectMarks, Bool, String)]) -> b) -> b -> MarkSheet -> b
foldScore func accumulator mksheet = 
    let validscore = map (\(studname,scorelist)->(studname,foldl' (\arr (subname,MkSubjectMarks mark) -> 
                    if (mark < 0)
                    then arr ++ [(subname,MkSubjectMarks mark,False,"negative score")]
                    else if (mark > 100)
                    then arr ++ [(subname,MkSubjectMarks mark,False,"greater than 100")]
                    else if (subname `notElem` subjects)
                    then arr ++ [(subname,MkSubjectMarks mark,False,"invalid subject name")]
                    else arr ++ [(subname,MkSubjectMarks mark,True,"Valid")]) [] scorelist)) mksheet
        dup = group (sort (map (\(x,y) -> x) validscore))
        getDup = map (\x-> head x) (filter (\x -> (length x) > 1) dup)
        setDup = map (\(x,y) -> if (x `elem` getDup) then (x,False,y) else (x,True,y)) validscore
    in foldl' (\acc x -> func acc x) accumulator setDup

subAvg :: MarkSheet -> SubjectName -> Float
subAvg mksheet subjname = 
    let (ttl,len) = foldScore (\(sum,num) (sname,k,scorelist) -> if k
            then
                foldl' (\(s,n) (sub,MkSubjectMarks mks,flag,msg) -> if (sub == subjname) && flag
                    then (s+mks,n+1)
                    else (s,n)) (sum,num) scorelist
            else (sum,num)
            ) (0,0) mksheet
    in fromIntegral ttl / fromIntegral len

calculateSd :: MarkSheet -> SubjectName -> Float 
calculateSd mksheet subjname =
    let avg :: Float = subAvg mksheet subjname
        mksarr :: [Float] = foldScore (\arr (sname,k,scorelist) -> 
                foldl' (\ar (sub,MkSubjectMarks mks,flag,msg) -> if (sub == subjname) && flag
                then ar ++ [((**) (fromIntegral mks - avg) 2)]
                else ar) arr scorelist
            ) [] mksheet
        variance :: Float = (sum mksarr) / fromIntegral (length mksarr)
    in sqrt variance

duplicateNames :: MarkSheet -> [StudentName]
duplicateNames mksheet = 
    let list = foldScore (\arr (a,k,b) -> if (k==False) then arr ++ [a] else arr) [] mksheet
    in nub list

invalidScores :: MarkSheet -> [(StudentName,[(SubjectName,SubjectMarks,Bool,String)])]
invalidScores mksheet =
    let subs = foldScore (\arr (stuname,k,scorelist) -> arr ++ [(stuname,(filter (\(sub,mks,flag,msg)-> flag==False)) scorelist)]) [] mksheet
    in subs


--Part2
studentsInSubject :: MarkSheet -> [(SubjectName,[StudentName])]
studentsInSubject mksheet = 
    let iv = map (\ s -> (s,[])) subjects
    in map (\ini -> (foldScore (\(sub,arr) (sname,k,scorelist)-> (sub,if k
                then if length (filter (\(subj,mk,flag,msg) -> (subj == sub) && (flag==True)) scorelist) > 0
                    then arr ++ [sname]
                    else arr
                else arr)
                ) ini mksheet)) iv

subjectsInExam :: MarkSheet -> [([SubjectName],[StudentName])]
subjectsInExam  mksheet =
    let namelist = foldScore (\arr (sname,k,scorelist)-> if k
                    then arr ++ [(sname,foldl'(\ ar (sub,mks,flag,msg) -> if flag
                        then ar ++ [sub]
                        else ar
                    ) [] scorelist)]
                    else arr
                ) [] mksheet
        sublist = map (\ini -> (foldScore (\(sub,arr) (sname,k,scorelist)-> (sub,if k
                    then if length (filter (\(subj,mk,flag,msg) -> (subj == sub) && (flag==True)) scorelist) > 0
                        then arr ++ [sname]
                        else arr
                    else arr)
                    ) ini mksheet)) (map (\ s -> (s,[])) subjects)
        newname = delete [] (nub (map (\(x,y) -> y) namelist))
        newsub =  delete [] (nub (map (\(x,y) -> y) sublist))
        emptyele = filter (\(x,y)-> y == []) sublist
        emptylist = map (\(x,y) -> ([x],y)) emptyele
        newlist = zip newname newsub
    in newlist ++ emptylist