{-# LANGUAGE ScopedTypeVariables #-}
module Chapter2.ExamScore where
import Data.List

type StudentName = String
type SubjectName = String
type SubjectMarks = Integer
type MarkSheet = [(String,[(String,Integer)])]
mksheet :: MarkSheet
mksheet = [("Saurabh Nanda",[("English", 84), ("Chemistry", 80), ("Physics", 95), ("Geography", 75)]),("John Doe", [("Chemistry", 80), ("Physics", 95), ("Geography", 75)]),("Jane Doe", [("Chemistry", 66), ("Phsyics", 33), ("Geography", 56)]), ("John Doe", [("Chemistry", 90), ("Economics", 45), ("Geography", 56)]),("Bahubali", [("Hindi", 45), ("Biology", -90), ("Geography", -75)]),("Rajnikant", [("Tamil", 110), ("Biology", 100), ("Geography", 100)])]
subjects :: [SubjectName]
subjects = [ "English","Geography","Physics","Chemistry","Economics","Computer Science"]

--Part 1

foldScore :: (b -> (StudentName,Maybe [(SubjectName,SubjectMarks, Bool, String)]) -> b) -> b -> MarkSheet -> b
foldScore func accumulator marksheet = 
    let validscore = map (\(studname,scorelist)->(studname,foldl' (\arr (subname,mark) -> 
                    if (mark < 0)
                    then arr ++ [(subname,mark,False,"negative score")]
                    else if (mark > 100)
                    then arr ++ [(subname,mark,False,"greater than 100")]
                    else if (subname `notElem` subjects)
                    then arr ++ [(subname,mark,False,"invalid subject name")]
                    else arr ++ [(subname,mark,True,"Valid")]) [] scorelist)) marksheet
        dup = group (sort (map (\(x,_) -> x) validscore))
        getDup = map (\x-> head x) (filter (\x -> (length x) > 1) dup)
        setDup = map (\(x,y) -> if (x `elem` getDup) then (x,Nothing) else (x,Just y)) validscore
    in foldl' (\acc x -> func acc x) accumulator setDup

subAvg :: MarkSheet -> SubjectName -> Float
subAvg marksheet subjname = 
    let (ttl,len) = foldScore (\(sumc,num) (_,scorelist) ->
            case scorelist of
                Nothing -> (sumc,num)
                Just scl -> foldl' (\(s,n) (sub,mks,flag,_) -> if (sub == subjname) && flag
                        then (s+mks,n+1)
                        else (s,n)) (sumc,num) scl
                    ) (0,0) marksheet
    in fromIntegral ttl / len

calculateSd :: MarkSheet -> SubjectName -> Float 
calculateSd marksheet subjname =
    let avg = subAvg mksheet subjname
        mksarr = foldScore (\arr (_,scorelist) -> 
                    case scorelist of
                        Just scl -> foldl' (\ar (sub,mks,flag,_) -> 
                                    if (sub == subjname) && flag
                                        then ar ++ [((**) (fromIntegral mks - avg) 2)]
                                    else ar) arr scl
                        Nothing -> arr
            ) [] marksheet
        variance = (sum mksarr) / fromIntegral (length mksarr)
    in sqrt variance


duplicateNames :: MarkSheet -> [StudentName]
duplicateNames marksheet = 
    let list = foldScore (\arr (a,sc) ->
            case sc of
                Nothing -> arr
                Just _ -> arr ++ [a]
            ) [] marksheet
    in nub list

invalidScores :: MarkSheet -> [(StudentName,[(SubjectName,SubjectMarks,Bool,String)])]
invalidScores marksheet =
    let subs = foldScore (\arr (stuname,Just scorelist) -> arr ++ [(stuname,(filter (\(_,_,flag,_) -> flag == False)) scorelist)]) [] marksheet
    in subs


--Part2
studentsInSubject :: MarkSheet -> [(SubjectName,[StudentName])]
studentsInSubject marksheet = 
    let iv = map (\ s -> (s,[])) subjects
    in map (\ini -> (foldScore (\(sub,arr) (sname,scorelist)-> 
        (sub,case scorelist of
                Nothing -> arr
                Just scl -> if length (filter (\(subj,_,flag,_) -> (subj == sub) && (flag==True)) scl) > 0
                            then arr ++ [sname]
                            else arr)
                ) ini marksheet)) iv
    
subjectsInExam :: MarkSheet -> [([SubjectName],[StudentName])]
subjectsInExam  marksheet =
    let namelist = foldScore (\arr (sname,scorelist)-> 
                    case scorelist of
                        Nothing -> arr
                        Just scl -> arr ++ [(sname,foldl'(\ ar (sub,_,flag,_) -> 
                                    if flag
                                        then ar ++ [sub]
                                    else ar
                                    ) [] scl)]
                            ) [] marksheet
        sublist = studentsInSubject marksheet
        newname = delete [] (nub (map (\(_,y) -> y) namelist))
        newsub =  delete [] (nub (map (\(_,y) -> y) sublist))
        emptyele = filter (\(_,y)-> y == []) sublist
        emptylist = map (\(x,y) -> ([x],y)) emptyele
        newlist = zip newname newsub
    in newlist ++ emptylist