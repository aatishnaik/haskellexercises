{-# LANGUAGE ScopedTypeVariables #-}
module Chapter2.ExamScore where
import Data.Char
import Data.List

type StudentName = String
type SubjectName = String
type SubjectMarks = Integer
type MarkSheet = [(String,[(String,Integer)])]

mksheet = [("Saurabh Nanda",[("English", 84), ("Chemistry", 80), ("Physics", 95), ("Geography", 75)]),("John Doe", [("Chemistry", 80), ("Physics", 95), ("Geography", 75)]),("Jane Doe", [("Chemistry", 66), ("Phsyics", 33), ("Geography", 56)]), ("John Doe", [("Chemistry", 90), ("Economics", 45), ("Geography", 56)]),("Bahubali", [("Hindi", 45), ("Biology", -90), ("Geography", -75)]),("Rajnikant", [("Tamil", 110), ("Biology", 100), ("Geography", 100)])]
subjects = [ "English","Geography","Physics","Chemistry","Economics","Computer Science"]

--Part 1
foldScore :: ([(StudentName,Bool,[(SubjectName,SubjectMarks,Bool,String)])] -> b) -> b -> MarkSheet -> b
foldScore func accumulator mksheet =
    let validscore = map (\(studname,scorelist)->(studname,foldl' (\arr (subname,mark) -> 
                if (mark < 0)
                then arr ++ [(subname,mark,False,"negative score")]
                else if (mark > 100)
                then arr ++ [(subname,mark,False,"greater than 100")]
                else if not (subname `elem` subjects)
                then arr ++ [(subname,mark,False,"invalid subject name")]
                else arr ++ [(subname,mark,True,"Valid")]) [] scorelist)) mksheet
        dup = group (sort (map (\(x,y) -> x) validscore))
        getDup = map (\x-> head x) (filter (\x -> (length x) > 1) dup)
        setDup = map (\(x,y) -> if (x `elem` getDup) then (x,False,y) else (x,True,y)) validscore
    in func setDup

calculateScore :: ([SubjectMarks] -> b) -> MarkSheet -> SubjectName -> b
calculateScore calfunc mksheet subjname =
    let 
        subs = foldScore (\x -> foldl' (\arr (a,k,b) -> arr ++ [b] ) [] x) [] mksheet
        validsubs = concatMap (\scorelist -> (filter (\(sname,smks,key,msg) -> (sname==subjname) && (key == True)) scorelist)) subs
        scores = map (\(sname,smks,key,msg) -> smks) validsubs
    in calfunc scores

subAvg :: MarkSheet -> SubjectName -> Float
subAvg markSheet subjname = calculateScore (\x ->(fromIntegral (sum x) / fromIntegral (length x))) markSheet subjname 

calculateSd :: MarkSheet -> SubjectName -> Float 
calculateSd mksheet subname =
    let avg :: Float = calculateScore (\x ->(fromIntegral (sum x) / fromIntegral (length x))) mksheet subname
        mksArr :: [Float] = calculateScore (\arr ->(map (\ele -> ((**) ((fromIntegral ele)-avg) 2)) arr)) mksheet subname
        variance :: Float = (sum mksArr) / fromIntegral (length mksArr)
    in sqrt variance

duplicateNames :: MarkSheet -> [StudentName]
duplicateNames mksheet = 
    let list = foldScore (\x -> filter(\(a,k,b) -> k==False) x) [] mksheet
    in nub (map (\(a,k,b)->a) list)

invalidScores :: MarkSheet -> [(StudentName,[(SubjectName,SubjectMarks,String)])]
invalidScores mksheet =
    let subs = foldScore (\x -> (map (\(stuname,k,scorelist) -> (stuname,k,(filter (\(subname,smks,key,msg) -> (key == False)) scorelist)))) x) [] mksheet
        unisubs = filter (\(a,k,b) -> k) subs
        newsubs= map (\(stname,k,scorelist)-> (stname,map (\(subname,submks,key,msg)->(subname,submks,msg)) scorelist)) subs
    in newsubs


--Part2
calList :: ([(StudentName,[SubjectName])] -> [(SubjectName,[StudentName])]) -> MarkSheet  -> [(SubjectName,[StudentName])]
calList func mksheet =
    let 
        subs = foldScore (\x -> (map (\(stuname,k,scorelist) -> (stuname,k,(filter (\(subname,smks,key,msg) -> (key == True)) scorelist)))) x) [] mksheet
        newsubs = map (\(stname,k,scorelist)-> (stname,k,map (\(subname,submks,key,msg)->(subname,submks)) scorelist)) subs
        unisubs = filter (\(a,k,b) -> k) newsubs
        newlist = map (\(namestud,k,scorelist) -> (namestud,(map (\(namesub,mkssub) -> namesub) scorelist))) unisubs
    in func newlist

studentsInSubject :: MarkSheet -> [(SubjectName,[StudentName])]
studentsInSubject mksheet = calList (\newlist -> map (\s -> (s,(foldl' (\arr (namestud,namesub) -> 
            if (s `elem` namesub)
                then arr ++ [namestud]
                else arr
            ) [] newlist) )) subjects) mksheet
    
subjectsInExam :: MarkSheet -> [([SubjectName],[StudentName])]
subjectsInExam  mksheet =
    let namelist = calList (\x -> x) mksheet
        sublist = calList (\newlist -> map (\s -> (s,(foldl' (\arr (namestud,namesub) -> 
            if (s `elem` namesub)
                then arr ++ [namestud]
                else arr
            ) [] newlist) )) subjects) mksheet
        newname = delete [] (nub (map (\(x,y) -> y) namelist))
        newsub =  delete [] (nub (map (\(x,y) -> y) sublist))
        emptyele = filter (\(x,y)-> y == []) sublist
        emptylist = map (\(x,y) -> ([x],y)) emptyele
        newlist = zip newname newsub
    in newlist ++ emptylist