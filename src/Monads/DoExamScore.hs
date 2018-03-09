{-# LANGUAGE ScopedTypeVariables #-}
module Monads.ExamScore where
import Data.List
import Text.Read
type StudentName = String
type SubjectName = String
type SubjectMarks = Integer
type MarkSheet = [(String,[(String,Integer)])]

--Part 1
foldScore :: (b -> (StudentName,Bool,[(SubjectName,SubjectMarks, Bool, String)]) -> b) -> b -> MarkSheet -> [String] -> b
foldScore func accumulator mksheet subjects= 
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

subAvg :: MarkSheet -> SubjectName -> [String] -> Float
subAvg marksheet subjname subjects= 
    let (ttl,len) = foldScore (\(sumc,num) (_,k,scorelist) -> if k
            then
                foldl' (\(s,n) (sub,mks,flag,_) -> if (sub == subjname) && flag
                    then (s+mks,n+1)
                    else (s,n)) (sumc,num) scorelist
            else (sumc,num)
            ) (0,0) marksheet subjects
    in fromIntegral ttl / len

calculateSd :: MarkSheet -> SubjectName -> [String] -> Float 
calculateSd mksheet subjname subjects=
    let avg :: Float = subAvg mksheet subjname subjects
        mksarr :: [Float] = foldScore (\arr (_,_,scorelist) -> 
                foldl' (\ar (sub,mks,flag,_) -> if (sub == subjname) && flag
                then ar ++ [((**) (fromIntegral mks - avg) 2)]
                else ar) arr scorelist
            ) [] mksheet subjects
        variance :: Float = (sum mksarr) / fromIntegral (length mksarr)
    in sqrt variance

duplicateNames :: MarkSheet -> [String] -> [StudentName]
duplicateNames marksheet subjects= 
    let list = foldScore (\arr (a,k,_) -> if (k==False) then arr ++ [a] else arr) [] marksheet subjects
    in nub list

invalidScores :: MarkSheet -> [String] -> [(StudentName,[(SubjectName,SubjectMarks,Bool,String)])]
invalidScores marksheet subjects=
    let subs = foldScore (\arr (stuname,_,scorelist) -> arr ++ [(stuname,(filter (\(_,_,flag,_)-> flag==False)) scorelist)]) [] marksheet subjects
    in subs
--Part2
studentsInSubject :: MarkSheet -> [String] -> [(SubjectName,[StudentName])]
studentsInSubject marksheet subjects= 
    let iv = map (\ s -> (s,[])) subjects
    in map (\ini -> (foldScore (\(sub,arr) (sname,k,scorelist)-> (sub,if k
                then if length (filter (\(subj,_,flag,_) -> (subj == sub) && (flag==True)) scorelist) > 0
                    then arr ++ [sname]
                    else arr
                else arr)
                ) ini marksheet subjects)) iv
    
subjectsInExam :: MarkSheet -> [String] -> [([SubjectName],[StudentName])]
subjectsInExam  marksheet subjects=
    let namelist = foldScore (\arr (sname,k,scorelist)-> if k
                    then arr ++ [(sname,foldl'(\ ar (sub,_,flag,_) -> if flag
                        then ar ++ [sub]
                        else ar
                    ) [] scorelist)]
                    else arr
                ) [] marksheet subjects
        sublist = map (\ini -> (foldScore (\(sub,arr) (sname,k,scorelist)-> (sub,if k
                    then if length (filter (\(subj,_,flag,_) -> (subj == sub) && (flag==True)) scorelist) > 0
                        then arr ++ [sname]
                        else arr
                    else arr)
                    ) ini marksheet subjects)) (map (\ s -> (s,[])) subjects)
        newname = delete [] (nub (map (\(_,y) -> y) namelist))
        newsub =  delete [] (nub (map (\(_,y) -> y) sublist))
        emptyele = filter (\(_,y)-> y == []) sublist
        emptylist = map (\(x,y) -> ([x],y)) emptyele
        newlist = zip newname newsub
    in newlist ++ emptylist

-- Monads part starts here

-- main display function
displayMkList :: IO()
displayMkList = 
    do
        putStrLn "Enter filename:"
        mkFile <- getLine
        mkOp <-readFile mkFile
        putStrLn "Enter Subject filename:"
        sbFile<-getLine
        sublist<-readFile sbFile
        calcOptions mkOp sublist

-- function to recurse through case
calcOptions :: String -> String -> IO()
calcOptions mksheetOp subOp = 
    do
    putStrLn "Enter 1 for avg Marks\nEnter 2 for Standard deviation\nEnter 3 to get Duplicate names\nEnter 4 to get Invalid scores in subjects\nEnter 5 for Students taking exam in a subject\nEnter 6 for Students that have taken same exams"
    ch<-getLine
    let mkStrn = formatMksheet mksheetOp
        subjects = formatSub subOp
    case mkStrn of
        Right mkStr->
            case ((readMaybe ch)::Maybe Int) of
                        Just 1-> getSubAvg mkStr subjects
                        Just 2-> getStdDev mkStr subjects
                        Just 3-> getDuplicates mkStr subjects
                        Just 4-> getInvalidScores mkStr subjects
                        Just 5-> getStudentsInSubject mkStr subjects
                        Just 6-> getSubjectsInExam mkStr subjects
                        _-> putStrLn "Invalid Choice"
        Left mkStrn-> putStrLn mkStrn
    calcOptions mksheetOp subOp

-- wrapper functions to which display formatted output
getSubAvg :: MarkSheet -> [String] -> IO()
getSubAvg mksheet subjects = 
    do
    putStrLn "Enter subjectName:"
    subname<-getLine
    if subname `elem` subjects
    then putStrLn $ show (subAvg mksheet subname subjects)
    else putStrLn "Invalid SubjectName" >> getSubAvg mksheet subjects

getStdDev :: MarkSheet -> [String] -> IO()
getStdDev mksheet subjects = 
    do
    putStrLn "Enter subjectName:"
    subname<-getLine
    if subname `elem` subjects
    then putStrLn $ show (calculateSd mksheet subname subjects)
    else putStrLn "Invalid SubjectName" >> getStdDev mksheet subjects

getDuplicates :: MarkSheet -> [String] -> IO()
getDuplicates mksheet subjects = putStrLn (foldl' (\opStr nme -> 
    opStr ++ nme ++ "\n") "" (duplicateNames mksheet subjects))

getInvalidScores :: MarkSheet -> [String] -> IO()
getInvalidScores mksheet subjects = putStrLn (foldl' (\opstr (sname,list)-> 
    foldl' (\str (a,b,c,d)-> 
        str++sname++" | "++a++" | "++show b++" | "++show c++" | "++d++"\n") opstr list
    ) "" (invalidScores mksheet subjects))

getStudentsInSubject :: MarkSheet -> [String] -> IO()
getStudentsInSubject mksheet subjects = putStrLn (foldl' (\opstr (sname,list)-> 
    opstr ++"\n"++sname++" | " ++ foldl' (\str (studname)-> 
        str++studname++" ") "" list
    ) "" (studentsInSubject mksheet subjects))

getSubjectsInExam :: MarkSheet -> [String] -> IO()
getSubjectsInExam mksheet subjects = putStrLn (foldl' (\opstr (subList,snameList)-> 
        let subL = foldl' (\str (subname)-> 
                str++subname++" | ") "" subList
        in opstr ++"\n"++ subL ++ foldl' (\str (stname)-> 
            str++stname++" ") "" snameList
    ) "" (subjectsInExam mksheet subjects))

--converts to marsheet format
formatMksheet :: String -> MarkSheet
formatMksheet str = fMksheet 0 [] str
    where fMksheet flag mksheet remStr = 
            if remStr == ""
                then mksheet
            else
                let (mkRecord,remst)=break (=='\n') remStr
                    (sname,subremStr) = break (==',') mkRecord
                    (subname,mkremStr)= break (==',') (tail subremStr)
                    mark = (readMaybe (tail mkremStr)) :: Maybe Integer 
                    (sn,sublist) = if mksheet == []
                        then ("",[])
                        else last mksheet
                in if sn == sname
                    then fMksheet 0 ((init mksheet) ++ [(sn,sublist ++ [(subname,mark)])]) (tail remst)
                    else fMksheet 0 (mksheet ++ [(sname,[(subname,mark)])]) (tail remst)

--gets subject csvs in list of strings
formatSub :: String -> [String]
formatSub subOp = fSub [] subOp
    where fSub subs remstr = if remstr == ""
            then subs
            else
                let (subname,nxt) = (break (==',') remstr)
                    nxtrecord = if nxt== "" then "" else tail nxt
                in fSub (subs ++ [subname]) nxtrecord