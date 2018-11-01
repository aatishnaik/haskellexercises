{-# LANGUAGE ScopedTypeVariables #-}
module Casava.ExamScore where

import Data.List as DL
import Data.List.Split as DLS
import Text.Read as TR
import Data.Text as DT
import Data.ByteString.Char8 as DB
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

type StudentName = String
type SubjectName = String
type SubjectMarks = Integer
type MarkSheet = [(String,[(String,Integer)])]

--Part 1
foldScore :: (b -> (StudentName,Bool,[(SubjectName,SubjectMarks, Bool, String)]) -> b) -> b -> MarkSheet -> [String] -> b
foldScore func accumulator mksheet subjects= 
    let validscore = DL.map (\(studname,scorelist)->(studname,DL.foldl' (\arr (subname,mark) -> 
                    if (mark < 0)
                    then arr DL.++ [(subname,mark,False,"negative score")]
                    else if (mark > 100)
                    then arr DL.++ [(subname,mark,False,"greater than 100")]
                    else if (subname `DL.notElem` subjects)
                    then arr DL.++ [(subname,mark,False,"invalid subject name")]
                    else arr DL.++ [(subname,mark,True,"Valid")]) [] scorelist)) mksheet
        dup = DL.group (DL.sort (DL.map (\(x,_) -> x) validscore))
        getDup = DL.map (\x-> DL.head x) (DL.filter (\x -> (DL.length x) > 1) dup)
        setDup = DL.map (\(x,y) -> if (x `DL.elem` getDup) then (x,False,y) else (x,True,y)) validscore
    in DL.foldl' (\acc x -> func acc x) accumulator setDup

subAvg :: MarkSheet -> SubjectName -> [String] -> Float
subAvg marksheet subjname subjects= 
    let (ttl,len) = foldScore (\(sumc,num) (_,k,scorelist) -> if k
            then
                DL.foldl' (\(s,n) (sub,mks,flag,_) -> if (sub == subjname) && flag
                    then (s+mks,n+1)
                    else (s,n)) (sumc,num) scorelist
            else (sumc,num)
            ) (0,0) marksheet subjects
    in fromIntegral ttl / len

calculateSd :: MarkSheet -> SubjectName -> [String] -> Float 
calculateSd mksheet subjname subjects=
    let avg :: Float = subAvg mksheet subjname subjects
        mksarr :: [Float] = foldScore (\arr (_,_,scorelist) -> 
                DL.foldl' (\ar (sub,mks,flag,_) -> if (sub == subjname) && flag
                then ar DL.++ [((**) (fromIntegral mks - avg) 2)]
                else ar) arr scorelist
            ) [] mksheet subjects
        variance :: Float = (DL.sum mksarr) / fromIntegral (DL.length mksarr)
    in sqrt variance

duplicateNames :: MarkSheet -> [String] -> [StudentName]
duplicateNames marksheet subjects= 
    let list = foldScore (\arr (a,k,_) -> if (k==False) then arr DL.++ [a] else arr) [] marksheet subjects
    in nub list

invalidScores :: MarkSheet -> [String] -> [(StudentName,[(SubjectName,SubjectMarks,Bool,String)])]
invalidScores marksheet subjects=
    let subs = foldScore (\arr (stuname,_,scorelist) -> arr DL.++ [(stuname,(DL.filter (\(_,_,flag,_)-> flag==False)) scorelist)]) [] marksheet subjects
    in subs
--Part2
studentsInSubject :: MarkSheet -> [String] -> [(SubjectName,[StudentName])]
studentsInSubject marksheet subjects= 
    let iv = DL.map (\ s -> (s,[])) subjects
    in DL.map (\ini -> (foldScore (\(sub,arr) (sname,k,scorelist)-> (sub,if k
                then if DL.length (DL.filter (\(subj,_,flag,_) -> (subj == sub) && (flag==True)) scorelist) > 0
                    then arr DL.++ [sname]
                    else arr
                else arr)
                ) ini marksheet subjects)) iv
    
subjectsInExam :: MarkSheet -> [String] -> [([SubjectName],[StudentName])]
subjectsInExam  marksheet subjects=
    let namelist = foldScore (\arr (sname,k,scorelist)-> if k
                    then arr DL.++ [(sname,DL.foldl'(\ ar (sub,_,flag,_) -> if flag
                        then ar DL.++ [sub]
                        else ar
                    ) [] scorelist)]
                    else arr
                ) [] marksheet subjects
        sublist = DL.map (\ini -> (foldScore (\(sub,arr) (sname,k,scorelist)-> (sub,if k
                    then if DL.length (DL.filter (\(subj,_,flag,_) -> (subj == sub) && (flag==True)) scorelist) > 0
                        then arr DL.++ [sname]
                        else arr
                    else arr)
                    ) ini marksheet subjects)) (DL.map (\ s -> (s,[])) subjects)
        newname = delete [] (nub (DL.map (\(_,y) -> y) namelist))
        newsub =  delete [] (nub (DL.map (\(_,y) -> y) sublist))
        emptyele = DL.filter (\(_,y)-> y == []) sublist
        emptylist = DL.map (\(x,y) -> ([x],y)) emptyele
        newlist = DL.zip newname newsub
    in newlist DL.++ emptylist

-- Monads part starts here

-- main display function
displayMkList :: IO()
displayMkList = Prelude.putStrLn "Enter filename:" >> 
                    Prelude.getLine >>= \mkFile -> (Prelude.readFile mkFile) >>= \mkOp ->
                        Prelude.putStrLn "Enter Subject filename:" >> 
                            Prelude.getLine >>= \sbFile -> (Prelude.readFile sbFile) >>= \sublist ->
                                calcOptions mkOp sublist

-- function to recurse through case
calcOptions :: String -> String -> IO()
calcOptions mksheetOp subOp = Prelude.putStrLn "Enter 1 for avg Marks\nEnter 2 for Standard deviation\nEnter 3 to get Duplicate names\nEnter 4 to get Invalid scores in subjects\nEnter 5 for Students taking exam in a subject\nEnter 6 for Students that have taken same exams">>
    Prelude.getLine >>= \ch ->
        let mkStr = formatMksheet mksheetOp
            subjects = formatSub subOp
        in case ((readMaybe ch)::Maybe Int) of
                Just 1-> getSubAvg mkStr subjects
                Just 2-> getStdDev mkStr subjects
                Just 3-> getDuplicates mkStr subjects
                Just 4-> getInvalidScores mkStr subjects
                Just 5-> getStudentsInSubject mkStr subjects
                Just 6-> getSubjectsInExam mkStr subjects
                _-> Prelude.putStrLn "Invalid Choice"
            >> calcOptions mksheetOp subOp

-- wrapper functions to which display formatted output
getSubAvg :: MarkSheet -> [String] -> IO()
getSubAvg mksheet subjects = Prelude.putStrLn "Enter subjectName:" >>
    Prelude.getLine >>= \subname-> if subname `DL.elem` subjects
        then Prelude.putStrLn $ show (subAvg mksheet subname subjects)
        else Prelude.putStrLn "Invalid SubjectName" >> getSubAvg mksheet subjects

getStdDev :: MarkSheet -> [String] -> IO()
getStdDev mksheet subjects = Prelude.putStrLn "Enter subjectName:" >>
    Prelude.getLine >>= \subname-> if subname `DL.elem` subjects
        then Prelude.putStrLn $ show (calculateSd mksheet subname subjects)
        else Prelude.putStrLn "Invalid SubjectName" >> getStdDev mksheet subjects

getDuplicates :: MarkSheet -> [String] -> IO()
getDuplicates mksheet subjects = Prelude.putStrLn (DL.foldl' (\opStr nme -> 
    opStr DL.++ nme DL.++ "\n") "" (duplicateNames mksheet subjects))

getInvalidScores :: MarkSheet -> [String] -> IO()
getInvalidScores mksheet subjects = Prelude.putStrLn (DL.foldl' (\opstr (sname,list)-> 
    DL.foldl' (\str (a,b,c,d)-> 
        str DL.++ sname DL.++" | " DL.++ a DL.++" | "DL.++show b DL.++" | "DL.++show c DL.++" | "DL.++ d DL.++"\n") opstr list
    ) "" (invalidScores mksheet subjects))

getStudentsInSubject :: MarkSheet -> [String] -> IO()
getStudentsInSubject mksheet subjects = Prelude.putStrLn (DL.foldl' (\opstr (sname,list)-> 
    opstr DL.++"\n"DL.++ sname DL.++" | " DL.++ DL.foldl' (\str (studname)-> 
        str DL.++studname DL.++" ") "" list
    ) "" (studentsInSubject mksheet subjects))

getSubjectsInExam :: MarkSheet -> [String] -> IO()
getSubjectsInExam mksheet subjects = Prelude.putStrLn (DL.foldl' (\opstr (subList,snameList)-> 
        let subL = DL.foldl' (\str (subname)-> 
                str DL.++subname DL.++" | ") "" subList
        in opstr DL.++"\n"DL.++ subL DL.++ DL.foldl' (\str (stname)-> 
            str DL.++stname DL.++" ") "" snameList
    ) "" (subjectsInExam mksheet subjects))

--gets 1 record from string of mksheet
mkSheetRec :: String -> (String,String,Integer,String)
mkSheetRec fileStr =
    let (name,str) = (DL.break (==',') fileStr)
        (subname,str1) = (DL.break (==',') (DL.tail str))
        (mark,str2) = (DL.break (==',') (DL.tail str1))
    in (name,subname,(read mark) :: Integer,
        if str2 /= "" then DL.tail str2 else "")

--converts to marsheet format
formatMksheet :: String -> MarkSheet
formatMksheet str = 
    let lineStr = DLS.splitOn "\n" str
        recStr = DL.map (\s -> DLS.splitOn "," s) lineStr
    in DL.foldl' (\msheet (sname:subname:mark:[])-> case ((readMaybe mark) :: Maybe Integer) of
                    Just mk -> if msheet==[]
                        then msheet DL.++[(sname,[(subname,mk)])]
                        else let (sn,mkl) = DL.last msheet
                            in if sn==sname 
                                    then (DL.init msheet)DL.++[(sname,mkl DL.++[(subname,mk)])]
                                    else msheet DL.++ [(sname,mkl DL.++[(subname,mk)])]
                    Nothing -> error "Marks not in number"
        ) [] recStr

--gets subject csvs in list of strings
formatSub :: String -> [String]
formatSub subOp = fSub [] subOp
    where fSub subs remstr = if remstr == ""
            then subs
            else
                let (subname,nxt) = (DL.break (==',') remstr)
                    nxtrecord = if nxt== "" then "" else DL.tail nxt
                in fSub (subs DL.++ [subname]) nxtrecord