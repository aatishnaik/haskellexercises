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

foldScore :: (b -> (StudentName,Bool,[(SubjectName,SubjectMarks, Bool, String)]) -> b) -> b -> MarkSheet -> b
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
    in foldl' (\acc x -> func acc x) accumulator setDup

subAvg :: MarkSheet -> SubjectName -> Float
subAvg markSheet subjname = 
    let (ttl,len) = foldScore (\(sum,num) (sname,k,scorelist) -> if k
            then
                foldl' (\(s,n) (sub,mks,flag,msg) -> if (sub == subjname) && flag
                    then (s+mks,n+1)
                    else (s,n)) (sum,num) scorelist
            else (sum,num)
            ) (0,0) mksheet
    in fromIntegral ttl / fromIntegral len

calculateSd :: MarkSheet -> SubjectName -> Float 
calculateSd mksheet subjname =
    let avg :: Float = subAvg mksheet subjname
        mksarr :: [Float] = foldScore (\arr (sname,k,scorelist) -> 
                foldl' (\ar (sub,mks,flag,msg) -> if (sub == subjname) && flag
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
calList :: ([(StudentName,[SubjectName])] -> [(SubjectName,[StudentName])]) -> MarkSheet  -> [(SubjectName,[StudentName])]
calList func mksheet = foldScore (\arr (sname,k,scorelist)-> if k
                then arr ++ [(sname,foldl'(\ ar (sub,mks,flag,msg) -> if flag
                    then ar ++ [sub]
                    else ar
                ) [] scorelist)]
                else arr
            ) [] mksheet

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