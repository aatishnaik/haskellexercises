module Chapter3.ExamScore2 where
import Data.Char
import Data.List
        
data StudentName = MkStudentName String deriving Show
data SubjectName = MkSubjectName String deriving Show
data SubjectMarks = MkSubjectMarks Int deriving Show
data SubjectArr= MkSubjectArr [SubjectName] deriving Show
data StudentArr= MkStudentArr [StudentName] deriving Show
data MarkSheet = MkMarkSheet [(StudentName,[(SubjectName,SubjectMarks)])] deriving Show

subAvg :: MarkSheet -> SubjectArr -> SubjectName -> Float
subAvg (MkMarkSheet mksheet) (MkSubjectArr subjects) (MkSubjectName subname) = 
    let validSubs = allValidSubjects (MkMarkSheet mksheet) (MkSubjectArr subjects)
        subs = map (\x -> (snd x)) validSubs
        subarr = map (\x -> (filter (\(MkSubjectName namesb,markssub) -> namesb == subname) x )) subs
        scoresarr = concat subarr
        scores = map (\(namesub,markssub) -> markssub) scoresarr
        tarr = map (\(MkSubjectMarks x)-> x) scores
    in fromIntegral (sum tarr) / fromIntegral(length tarr)


filterSubjects :: Bool -> MarkSheet -> SubjectArr -> StudentName -> [(SubjectName,SubjectMarks,String)]
filterSubjects filterFn (MkMarkSheet markSheet) (MkSubjectArr subjects) (MkStudentName studName) = 
    let subs = filter (\ (MkStudentName studentName, scoreList) -> studentName == studName) markSheet
        subjectList = snd (head subs)
        validSubs = map (\x -> (checkScore (MkMarkSheet markSheet) (MkSubjectArr subjects) (MkStudentName studName) (fst x))) subjectList
        list = filter (\ (namesub,markssub,flag,msg) -> (filterFn == flag)) validSubs
    in map (\ (namesub,markssub,flag,msg) -> (namesub,markssub,msg)) list

allInvalidSubjects :: MarkSheet -> SubjectArr -> [(StudentName,[(SubjectName,SubjectMarks,String)])]
allInvalidSubjects (MkMarkSheet markSheet) (MkSubjectArr subjects) =
    let invalidNames = map (\x -> (fst x,(filterSubjects False (MkMarkSheet markSheet) (MkSubjectArr subjects) (fst x)))) markSheet
    in invalidNames

allValidSubjects :: MarkSheet -> SubjectArr -> [(StudentName,[(SubjectName,SubjectMarks)])]
allValidSubjects (MkMarkSheet markSheet) (MkSubjectArr subjects) =
    let validNames = map (\x -> (fst x,(filterSubjects True (MkMarkSheet markSheet) (MkSubjectArr subjects) (fst x)))) markSheet
    in map (\(namestud,score) -> (namestud,(map (\(namesub,markssub,msg) -> (namesub,markssub)) score))) validNames


checkScore :: MarkSheet -> SubjectArr -> StudentName -> SubjectName -> (SubjectName,SubjectMarks,Bool,String)
checkScore (MkMarkSheet mksheet) (MkSubjectArr subjects) (MkStudentName studname) (MkSubjectName subname) =  
    let subOfStudent = snd (head (filter (\((MkStudentName x),y) -> x == studname) mksheet))
        scr = snd (head (filter (\(MkSubjectName x,y) -> x == subname) subOfStudent))
        mark = head (map (\(MkSubjectMarks scr) -> scr) [scr])
        subarr = map (\(MkSubjectName x)->x) subjects
    in if (mark < 0)
        then (MkSubjectName subname,MkSubjectMarks mark,False,"negative score")
        else if (mark > 100)
        then (MkSubjectName subname,MkSubjectMarks mark,False,"greater than 100")
        else if not (subname `elem` subarr)
        then (MkSubjectName subname,MkSubjectMarks mark,False,"invalid subject name")
        else (MkSubjectName subname,MkSubjectMarks mark,True,"Valid")


filterNames :: ([String] -> Bool) -> MarkSheet -> [StudentName]
filterNames filterFn (MkMarkSheet markSheet) =
    let studentNames = map (\ (MkStudentName studentName, scoreList) -> studentName) markSheet
        groupedStudentNames = group (sort studentNames)
        filterGroups = filter (\x -> filterFn x) groupedStudentNames
    in map (\x -> (MkStudentName (head x))) filterGroups
    
uniqueNames :: MarkSheet -> [StudentName]
uniqueNames (MkMarkSheet markSheet) = filterNames (\x -> length x == 1) (MkMarkSheet markSheet)
        
duplicateNames :: MarkSheet -> [StudentName]
duplicateNames (MkMarkSheet markSheet) = filterNames (\x -> length x > 1) (MkMarkSheet markSheet)

calculateSd :: MarkSheet -> SubjectArr -> SubjectName -> Float 
calculateSd (MkMarkSheet mksheet) (MkSubjectArr subjects) (MkSubjectName subname) =
    let avg = subAvg (MkMarkSheet mksheet) (MkSubjectArr subjects) (MkSubjectName subname)
        subScores = map (\x -> snd x) (allValidSubjects (MkMarkSheet mksheet) (MkSubjectArr subjects))
        subarr = map (\x -> (filter (\(MkSubjectName namesub,MkSubjectMarks markssub) -> namesub == subname) x )) subScores
        mksArr = map (\(MkSubjectName x,MkSubjectMarks y) -> (fromIntegral y)-avg) (concat subarr)
        squareArr = map (\x -> (x*x)) mksArr
        variance = (sum squareArr) / fromIntegral (length squareArr)
    in sqrt variance

allValidNames :: MarkSheet -> SubjectArr -> [(StudentName,[(SubjectName,SubjectMarks)])]
allValidNames (MkMarkSheet mksheet) (MkSubjectArr subjects) =
    let newmksheet = allValidSubjects (MkMarkSheet mksheet) (MkSubjectArr subjects)
        duplicateArr = map (\(MkStudentName x) -> x) (duplicateNames (MkMarkSheet mksheet))
    in filter (\(MkStudentName x,y) -> x `notElem` duplicateArr) newmksheet

studentsInSubject :: MarkSheet -> SubjectArr -> [(String,[String])]
studentsInSubject (MkMarkSheet mksheet) (MkSubjectArr subjects) =
    let vList = allValidNames (MkMarkSheet mksheet) (MkSubjectArr subjects)
        newlist = map (\(MkStudentName x,y) -> (x,(map (\(MkSubjectName namesub,MkSubjectMarks mkssub) -> namesub) y))) vList
        subArr = map (\(MkSubjectName x)->x) subjects
        arr = map (\s -> (s,(foldl' (\arr (namestud,namesub) -> 
            if (s `elem` namesub)
                then arr ++ [namestud]
                else arr
            ) [] newlist) )) subArr
    in arr

subjectsInExam :: MarkSheet -> SubjectArr -> [([String],[String])]
subjectsInExam  (MkMarkSheet mksheet) (MkSubjectArr subjects) =
    let namelist = map (\(MkStudentName namestud,scorelist) -> (namestud,(map (\(MkSubjectName namesub,MkSubjectMarks mkssub) -> namesub) scorelist))) (allValidNames (MkMarkSheet mksheet) (MkSubjectArr subjects))
        sublist = studentsInSubject (MkMarkSheet mksheet) (MkSubjectArr subjects)
        newname = delete [] (nub (map (\(x,y) -> y) namelist))
        newsub =  delete [] (nub (map (\(x,y) -> y) sublist))
        emptyele = filter (\(x,y)-> y == []) sublist
        emptylist = map (\(x,y) -> ([x],y)) emptyele
        newlist = zip newname newsub
    in newlist ++ emptylist

