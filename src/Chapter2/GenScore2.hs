{-# LANGUAGE ScopedTypeVariables #-}
module Chapter2.ExamScore where
import Data.Char
import Data.List

type StudentName = String
type SubjectName = String
type SubjectMarks = Int
type MarkSheet = [(String,[(String,Int)])]

--Part 1
filterNames :: ([StudentName] -> Bool) -> MarkSheet -> [StudentName]
filterNames filterFn markSheet =
    let studentNames = map (\ (studentName, scoreList) -> studentName) markSheet
        groupedStudentNames = group (sort studentNames)
        filterGroups = filter (\x -> filterFn x) groupedStudentNames
    in map (\x -> head x) filterGroups

checkScore :: ([(StudentName,[(SubjectName,SubjectMarks,Bool,String)])] -> b) -> MarkSheet -> [SubjectName] -> b
checkScore func mksheet subjects = 
    let duplicatenames = filterNames (\x -> length x > 1) mksheet
        newmksheet = filter (\(x,y)-> x `notElem` duplicatenames) mksheet
        validscore = map (\(studname,scorelist)->(studname,foldl' (\arr (subname,mark) -> 
                if (mark < 0)
                        then arr ++ [(subname,mark,False,"negative score")]
                        else if (mark > 100)
                        then arr ++ [(subname,mark,False,"greater than 100")]
                        else if not (subname `elem` subjects)
                        then arr ++ [(subname,mark,False,"invalid subject name")]
                        else arr ++ [(subname,mark,True,"Valid")]) [] scorelist)) newmksheet
    in func validscore

calculateScore :: ([SubjectMarks] -> b) -> MarkSheet -> [SubjectName] -> SubjectName -> b
calculateScore calfunc mksheet subjects subjname =
    let subs = checkScore (\x -> map(\(a,b) -> b) x) mksheet subjects
        validsubs = concatMap (\scorelist -> (filter (\(sname,smks,key,msg) -> (sname==subjname) && (key == True)) scorelist)) subs
        scores = map (\(sname,smks,key,msg) -> smks) validsubs
    in calfunc scores

subAvg :: MarkSheet -> [SubjectName] -> SubjectName -> Float
subAvg markSheet subjects subjname = calculateScore (\x ->(fromIntegral (sum x) / fromIntegral (length x))) markSheet subjects subjname 

calculateSd :: MarkSheet -> [SubjectName] -> SubjectName -> Float 
calculateSd mksheet subjects subname =
    let avg :: Float = calculateScore (\x ->(fromIntegral (sum x) / fromIntegral (length x))) mksheet subjects subname
        mksArr :: [Float] = calculateScore (\arr ->(map (\ele -> ((**) ((fromIntegral ele)-avg) 2)) arr)) mksheet subjects subname
        variance :: Float = (sum mksArr) / fromIntegral (length mksArr)
    in sqrt variance

duplicateNames :: MarkSheet -> [StudentName]
duplicateNames markSheet = filterNames (\x -> length x > 1) markSheet

invalidScores :: MarkSheet -> [SubjectName] -> [(StudentName,[(SubjectName,SubjectMarks,String)])]
invalidScores mksheet subjects =
    let subs = checkScore (\x -> (map (\(stuname,scorelist) -> (stuname,(filter (\(subname,smks,key,msg) -> (key == False)) scorelist)))) x) mksheet subjects
        newsubs= map (\(stname,scorelist)-> (stname,map (\(subname,submks,key,msg)->(subname,submks,msg)) scorelist)) subs
    in newsubs




--Part2
calList :: ([(StudentName,[SubjectName])] -> [(SubjectName,[StudentName])]) -> MarkSheet -> [SubjectName] -> [(SubjectName,[StudentName])]
calList func mksheet subjects =
    let subs = checkScore (\x -> (map (\(stuname,scorelist) -> (stuname,(filter (\(subname,smks,key,msg) -> (key == True)) scorelist)))) x) mksheet subjects
        newsubs = map (\(stname,scorelist)-> (stname,map (\(subname,submks,key,msg)->(subname,submks)) scorelist)) subs
        newlist = map (\(namestud,scorelist) -> (namestud,(map (\(namesub,mkssub) -> namesub) scorelist))) newsubs
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


    