module Chapter2.ExamScore2 where
import Data.Char
import Data.List

type StudentName = String
type SubjectName = String
type SubjectMarks = Int
type MarkSheet = [(String,[(String,Int)])]

calculateScore :: ([SubjectMarks] -> Float) -> MarkSheet -> [SubjectName] -> SubjectName -> Float
calculateScore calfunc markSheet subjects subjname =
    let validSubs = allValidNames markSheet subjects
        subs = map (\x -> (snd x)) validSubs
        scoresarr = concatMap (\x -> (filter (\(namesub,markssub) -> namesub == subjname) x )) subs
        scores = map (\(namesub,markssub) -> markssub) scoresarr
    in calfunc scores

subAvg :: MarkSheet -> [SubjectName] -> SubjectName -> Float
subAvg markSheet subjects subjname = calculateScore (\x ->(fromIntegral (sum x) / fromIntegral (length x))) markSheet subjects subjname 

calculateSd :: MarkSheet -> [SubjectName] -> SubjectName -> Float 
calculateSd mksheet subjects subname =
    let avg = calculateScore (\x ->(fromIntegral (sum x) / fromIntegral (length x))) mksheet subjects subname
        subScores = map (\x -> snd x) (allValidSubjects mksheet subjects)
        subarr = concatMap (\x -> (filter (\(namesub,markssub) -> namesub == subname) x )) subScores
        mksArr = map (\x -> (fromIntegral(snd x))-avg) subarr
        squareArr = map (\x -> (x*x)) mksArr
        variance = (sum squareArr) / fromIntegral (length squareArr)
    in sqrt variance



filterNames :: ([StudentName] -> Bool) -> MarkSheet -> [StudentName]
filterNames filterFn markSheet =
    let studentNames = map (\ (studentName, scoreList) -> studentName) markSheet
        groupedStudentNames = group (sort studentNames)
        filterGroups = filter (\x -> filterFn x) groupedStudentNames
    in map (\x -> head x) filterGroups

uniqueNames :: MarkSheet -> [StudentName]
uniqueNames markSheet = filterNames (\x -> length x == 1) markSheet

duplicateNames :: MarkSheet -> [StudentName]
duplicateNames markSheet = filterNames (\x -> length x > 1) markSheet



filterSubjects :: Bool -> MarkSheet -> [SubjectName] -> StudentName -> [(SubjectName,SubjectMarks,String)]
filterSubjects filterFn markSheet subjects studName =
    let subs = filter (\ (studentName, scoreList) -> studentName == studName) markSheet
        subjectList = snd (head subs)
        validSubs = map (\x -> (checkScore markSheet subjects studName (fst x))) subjectList
        list = filter (\ (namesub,markssub,flag,msg) -> (filterFn == flag)) validSubs
    in map (\ (namesub,markssub,flag,msg) -> (namesub,markssub,msg)) list

allInvalidSubjects :: MarkSheet -> [SubjectName] -> [(StudentName,[(SubjectName,SubjectMarks,String)])]
allInvalidSubjects markSheet subjects =
    let invalidNames = map (\x -> (fst x,(filterSubjects False markSheet subjects (fst x)))) markSheet
    in invalidNames

allValidSubjects :: MarkSheet -> [SubjectName] -> MarkSheet
allValidSubjects markSheet subjects =
    let validNames = map (\x -> (fst x,(filterSubjects True markSheet subjects (fst x)))) markSheet
    in map (\(namestud,score) -> (namestud,(map (\(namesub,markssub,msg) -> (namesub,markssub)) score))) validNames


    
checkScore :: MarkSheet -> [SubjectName] -> StudentName -> SubjectName -> (SubjectName,SubjectMarks,Bool,String)
checkScore mksheet subjects studname subname = 
    let subOfStudent = snd (head (filter (\x -> (fst x) == studname) mksheet))
        mark = snd (head (filter (\x -> (fst x == subname)) subOfStudent))
    in if (mark < 0)
    then (subname,mark,False,"negative score")
    else if (mark > 100)
    then (subname,mark,False,"greater than 100")
    else if not (subname `elem` subjects)
    then (subname,mark,False,"invalid subject name")
        else (subname,mark,True,"Valid")

allValidNames :: MarkSheet -> [SubjectName] -> MarkSheet
allValidNames mksheet subjects =
    let newmksheet = allValidSubjects mksheet subjects
    in filter (\x -> not ((fst x) `elem` (duplicateNames newmksheet))) newmksheet



calList :: ([(StudentName,[SubjectName])] -> [(SubjectName,[StudentName])]) -> MarkSheet -> [SubjectName] -> [(SubjectName,[StudentName])]
calList func mksheet subjects =
    let newlist = map (\(namestud,scorelist) -> (namestud,(map (\(namesub,mkssub) -> namesub) scorelist))) (allValidNames mksheet subjects)
    in func newlist

studentsInSubject :: MarkSheet -> [SubjectName] -> [(SubjectName,[StudentName])]
studentsInSubject mksheet subjects = calList (\newlist -> map (\s -> (s,(foldl' (\arr (namestud,namesub) -> 
            if (s `elem` namesub)
                then arr ++ [namestud]
                else arr
            ) [] newlist) )) subjects) mksheet subjects
    
subjectsInExam :: MarkSheet -> [SubjectName] -> [([SubjectName],[StudentName])]
subjectsInExam  mksheet subjects =
    let namelist = calList (\x -> x) mksheet subjects
        sublist = calList (\newlist -> map (\s -> (s,(foldl' (\arr (namestud,namesub) -> 
            if (s `elem` namesub)
                then arr ++ [namestud]
                else arr
            ) [] newlist) )) subjects) mksheet subjects
        newname = delete [] (nub (map (\(x,y) -> y) namelist))
        newsub =  delete [] (nub (map (\(x,y) -> y) sublist))
        emptyele = filter (\(x,y)-> y == []) sublist
        emptylist = map (\(x,y) -> ([x],y)) emptyele
        newlist = zip newname newsub
    in newlist ++ emptylist

    


    