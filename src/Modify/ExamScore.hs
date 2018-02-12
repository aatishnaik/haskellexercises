{-# LANGUAGE ScopedTypeVariables #-}
module Chapter2.ExamScore where
import Data.Char
import Data.List

type StudentName = String
type SubjectName = String
type SubjectMarks = Integer
type MarkSheet = [(String,[(String,Integer)])]

data Record = ValidSub SubjectName SubjectMarks
    | InvalidSub SubjectName SubjectMarks String
    deriving (Eq, Show, Ord)

mksheet :: MarkSheet
mksheet = [("Saurabh Nanda",[("English", 84), ("Chemistry", 80), ("Physics", 95), ("Geography", 75)]),("John Doe", [("Chemistry", 80), ("Physics", 95), ("Geography", 75)]),("Jane Doe", [("Chemistry", 66), ("Phsyics", 33), ("Geography", 56)]), ("John Doe", [("Chemistry", 90), ("Economics", 45), ("Geography", 56)]),("Bahubali", [("Hindi", 45), ("Biology", -90), ("Geography", -75)]),("Rajnikant", [("Tamil", 110), ("Biology", 100), ("Geography", 100)])]
subjects :: [SubjectName]
subjects = [ "English","Geography","Physics","Chemistry","Economics","Computer Science"]

--Part 1

foldScore :: (b -> (StudentName,Bool,[Record]) -> b) -> b -> MarkSheet -> b
foldScore func accumulator mksheet = foldl' (\acc x -> func acc x) accumulator setDup
    where 
        validscore = map (\(studname,scorelist)->(studname,foldl' (\arr (subname,mark) -> 
                    if (mark < 0)
                        then arr ++ [InvalidSub subname mark "negative score"]
                    else if (mark > 100)
                        then arr ++ [InvalidSub subname mark "greater than 100"]
                    else if (subname `notElem` subjects)
                        then arr ++ [InvalidSub subname mark "invalid subject name"]
                    else arr ++ [ValidSub subname mark]) [] scorelist)) mksheet
        dup = group $ sort $ map (\(x,y) -> x) validscore
        getDup = map (\x-> head x) (filter (\x -> (length x) > 1) dup)
        setDup = map (\(x,y) -> if (x `elem` getDup) then (x,False,y) else (x,True,y)) validscore


subAvg :: MarkSheet -> SubjectName -> Float
subAvg marksheet subjname = fromIntegral ttl / fromIntegral len
    where (ttl,len) = foldScore (\(sum1,num) (_,k,scorelist) -> if k
            then
                foldl' (\(s,n) x -> 
                    case x of
                        InvalidSub _ _ _ ->(s,n)
                        ValidSub _ mks -> (s+mks,n+1)
                    ) (sum1,num) scorelist
            else (sum1,num)
            ) (0,0) marksheet

calculateSd :: MarkSheet -> SubjectName -> Float 
calculateSd marksheet subjname =
    let avg :: Float = subAvg mksheet subjname
        mksarr :: [Float] = foldScore (\arr (_,_,scorelist) -> 
                foldl' (\ar x -> 
                    case x of
                        InvalidSub _ _ _ -> ar
                        ValidSub _ mks -> ar ++ [((**) (fromIntegral mks - avg) 2)]
                    ) arr scorelist
            ) [] marksheet
        variance :: Float = (sum mksarr) / fromIntegral (length mksarr)
    in sqrt variance

duplicateNames :: MarkSheet -> [StudentName]
duplicateNames marksheet = nub $foldScore (\arr (a,k,_) -> if (k==False) then arr ++ [a] else arr) [] marksheet

invalidScores :: MarkSheet -> [(StudentName,[Record])]
invalidScores marksheet = foldScore (\arr (stuname,_,scorelist) -> arr ++ [(stuname,(filter (\x -> 
            case x of
                ValidSub _ _ -> True
                InvalidSub _ _ _ -> False)) scorelist)]) [] marksheet

{--
--Part2
studentsInSubject :: MarkSheet -> [(SubjectName,[StudentName])]
studentsInSubject mksheet = 
    let iv = map (\ s -> (s,[])) subjects
    in map (\ini -> (foldScore (\(sub,arr) (sname,k,scorelist)-> (sub,if k
                then if length (filter (\x -> (subj == sub) && (flag==True)) scorelist) > 0
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
    in newlist ++ emptylist--}