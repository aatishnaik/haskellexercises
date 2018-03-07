{-# LANGUAGE ScopedTypeVariables #-}
module Monads.ExamScore where
import Data.List
import Text.Read
type StudentName = String
type SubjectName = String
type SubjectMarks = Integer
type MarkSheet = [(String,[(String,Integer)])]

--Part 1
foldScore :: (b -> (StudentName,Bool,[(SubjectName,SubjectMarks, Bool, String)]) -> b) -> b -> MarkSheet -> b
foldScore func accumulator mksheet = 
    let validscore = map (\(studname,scorelist)->(studname,foldl' (\arr (subname,mark) -> 
                    if (mark < 0)
                    then arr ++ [(subname,mark,False,"negative score")]
                    else if (mark > 100)
                    then arr ++ [(subname,mark,False,"greater than 100")]
                    else if (subname `notElem` subjects)
                    then arr ++ [(subname,mark,False,"invalid subject name")]
                    else arr ++ [(subname,mark,True,"Valid")]) [] scorelist)) mksheet
        dup = group (sort (map (\(x,_) -> x) validscore))
        getDup = map (\x-> head x) (filter (\x -> (length x) > 1) dup)
        setDup = map (\(x,y) -> if (x `elem` getDup) then (x,False,y) else (x,True,y)) validscore
    in foldl' (\acc x -> func acc x) accumulator setDup

subAvg :: MarkSheet -> SubjectName -> Float
subAvg marksheet subjname = 
    let (ttl,len) = foldScore (\(sumc,num) (_,k,scorelist) -> if k
            then
                foldl' (\(s,n) (sub,mks,flag,_) -> if (sub == subjname) && flag
                    then (s+mks,n+1)
                    else (s,n)) (sumc,num) scorelist
            else (sumc,num)
            ) (0,0) marksheet
    in fromIntegral ttl / len

calculateSd :: MarkSheet -> SubjectName -> Float 
calculateSd mksheet subjname =
    let avg :: Float = subAvg mksheet subjname
        mksarr :: [Float] = foldScore (\arr (_,_,scorelist) -> 
                foldl' (\ar (sub,mks,flag,_) -> if (sub == subjname) && flag
                then ar ++ [((**) (fromIntegral mks - avg) 2)]
                else ar) arr scorelist
            ) [] mksheet
        variance :: Float = (sum mksarr) / fromIntegral (length mksarr)
    in sqrt variance

duplicateNames :: MarkSheet -> [StudentName]
duplicateNames marksheet = 
    let list = foldScore (\arr (a,k,_) -> if (k==False) then arr ++ [a] else arr) [] marksheet
    in nub list

invalidScores :: MarkSheet -> [(StudentName,[(SubjectName,SubjectMarks,Bool,String)])]
invalidScores marksheet =
    let subs = foldScore (\arr (stuname,_,scorelist) -> arr ++ [(stuname,(filter (\(_,_,flag,_)-> flag==False)) scorelist)]) [] marksheet
    in subs

--Part2
studentsInSubject :: MarkSheet -> [(SubjectName,[StudentName])]
studentsInSubject marksheet = 
    let iv = map (\ s -> (s,[])) subjects
    in map (\ini -> (foldScore (\(sub,arr) (sname,k,scorelist)-> (sub,if k
                then if length (filter (\(subj,_,flag,_) -> (subj == sub) && (flag==True)) scorelist) > 0
                    then arr ++ [sname]
                    else arr
                else arr)
                ) ini marksheet)) iv
    
subjectsInExam :: MarkSheet -> [([SubjectName],[StudentName])]
subjectsInExam  marksheet =
    let namelist = foldScore (\arr (sname,k,scorelist)-> if k
                    then arr ++ [(sname,foldl'(\ ar (sub,_,flag,_) -> if flag
                        then ar ++ [sub]
                        else ar
                    ) [] scorelist)]
                    else arr
                ) [] marksheet
        sublist = map (\ini -> (foldScore (\(sub,arr) (sname,k,scorelist)-> (sub,if k
                    then if length (filter (\(subj,_,flag,_) -> (subj == sub) && (flag==True)) scorelist) > 0
                        then arr ++ [sname]
                        else arr
                    else arr)
                    ) ini marksheet)) (map (\ s -> (s,[])) subjects)
        newname = delete [] (nub (map (\(_,y) -> y) namelist))
        newsub =  delete [] (nub (map (\(_,y) -> y) sublist))
        emptyele = filter (\(_,y)-> y == []) sublist
        emptylist = map (\(x,y) -> ([x],y)) emptyele
        newlist = zip newname newsub
    in newlist ++ emptylist

mksheetL :: MarkSheet
mksheetL = [("Saurabh Nanda",[("English", 84), ("Chemistry", 80), ("Physics", 95), ("Geography", 75)]),("John Doe", [("Chemistry", 80), ("Physics", 95), ("Geography", 75)]),("Jane Doe", [("Chemistry", 66), ("Phsyics", 33), ("Geography", 56)]), ("John Doe", [("Chemistry", 90), ("Economics", 45), ("Geography", 56)]),("Bahubali", [("Hindi", 45), ("Biology", -90), ("Geography", -75)]),("Rajnikant", [("Tamil", 110), ("Biology", 100), ("Geography", 100)])]
subjects :: [SubjectName]
subjects = [ "English","Geography","Physics","Chemistry","Economics","Computer Science"]

displayMkList :: IO()
displayMkList = putStrLn "Enter 1 for avg Marks\nEnter 2 for Standard deviation\nEnter 3 to get Duplicate names\nEnter 4 to get Invalid scores in subjects\nEnter 5 for Students taking exam in a subject\nEnter 6 for Students that have taken same exams">>
                    getLine >>= \ch ->
                        case ((readMaybe ch)::Maybe Int) of
                            Just 1-> getSubAvg >> displayMkList
                            Just 2-> getStdDev >> displayMkList
                            Just 3-> getDuplicates >> displayMkList
                            Just 4-> getInvalidScores >> displayMkList
                            Just 5-> print (studentsInSubject mksheetL) >> displayMkList
                            Just 6-> print (subjectsInExam mksheetL) >> displayMkList
                            _-> putStrLn "Invalid Choice" >> displayMkList
getSubAvg :: IO()
getSubAvg = putStrLn "Enter subjectName:" >>
    getLine >>= \subname-> if subname `elem` subjects
        then putStrLn $ show (subAvg mksheetL subname)
        else putStrLn "Invalid SubjectName" >> getSubAvg

getStdDev :: IO()
getStdDev = putStrLn "Enter subjectName:" >>
    getLine >>= \subname-> if subname `elem` subjects
        then putStrLn $ show (calculateSd mksheetL subname)
        else putStrLn "Invalid SubjectName" >> getStdDev

getDuplicates :: IO()
getDuplicates = putStrLn (foldl' (\opStr nme -> opStr ++ nme ++ "\n") "" (duplicateNames mksheetL))

getInvalidScores :: IO()
getInvalidScores = putStrLn (foldl' (\opstr (sname,list)-> 
    foldl' (\str (a,b,c,d)-> 
        str++sname++" | "++a++" | "++show b++" | "++show c++" | "++d++"\n") opstr list
    ) "" (invalidScores mksheetL))

getStudentsInSubject :: IO()
getStudentsInSubject = _todo

getSubjectsInExam :: IO()
getSubjectsInExam = _todo