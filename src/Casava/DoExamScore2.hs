{-# LANGUAGE ScopedTypeVariables #-}
module Casava.ExamScore where

import Data.List as DL
import Text.Read as TR
import Data.Csv as DC
import Data.ByteString.Char8 as DB
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
type StudentName = String
type SubjectName = String
type SubjectMarks = Integer
type MarkSheet = [(String,[(String,Integer)])]

data EScore = EScore
    { studname   :: String
    , subname   :: String
    , marks :: Int
    }

{-
--applicative parser
instance FromNamedRecord EScore where
    parseNamedRecord r = EScore <$> r .: (DB.pack "Studname") <*> r .: (DB.pack "Subname") <*> r .: (DB.pack "Marks")
-}
instance FromNamedRecord EScore where
    --parseNamedRecord :: NamedRecord -> Parser EScore
    parseNamedRecord r =
        do
            studName <- DC.lookup r (DB.pack "Studname") :: Parser String
            subName <- DC.lookup r (DB.pack "Subname") :: Parser String
            subMarks <- DC.lookup r (DB.pack "Marks") :: Parser Int
            pure $ EScore { studname=studName,subname=subName,marks=subMarks }

--Part 1
foldScore :: (b -> (StudentName,Bool,[(SubjectName,SubjectMarks, Bool, String)]) -> b) -> b -> MarkSheet -> [String] -> b
foldScore func accumulator mksheet subjects= 
    let validscore = DL.map (\(studname,scorelist)->(studname,DL.foldl' (\arr (subname,mark) -> 
                    if (mark < 0)
                    then arr ++ [(subname,mark,False,"negative score")]
                    else if (mark > 100)
                    then arr ++ [(subname,mark,False,"greater than 100")]
                    else if (subname `DL.notElem` subjects)
                    then arr ++ [(subname,mark,False,"invalid subject name")]
                    else arr ++ [(subname,mark,True,"Valid")]) [] scorelist)) mksheet
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
                then ar ++ [((**) (fromIntegral mks - avg) 2)]
                else ar) arr scorelist
            ) [] mksheet subjects
        variance :: Float = (sum mksarr) / fromIntegral (DL.length mksarr)
    in sqrt variance

duplicateNames :: MarkSheet -> [String] -> [StudentName]
duplicateNames marksheet subjects= 
    let list = foldScore (\arr (a,k,_) -> if (k==False) then arr ++ [a] else arr) [] marksheet subjects
    in nub list

invalidScores :: MarkSheet -> [String] -> [(StudentName,[(SubjectName,SubjectMarks,Bool,String)])]
invalidScores marksheet subjects=
    let subs = foldScore (\arr (stuname,_,scorelist) -> arr ++ [(stuname,(DL.filter (\(_,_,flag,_)-> flag==False)) scorelist)]) [] marksheet subjects
    in subs

--Part2
studentsInSubject :: MarkSheet -> [String] -> [(SubjectName,[StudentName])]
studentsInSubject marksheet subjects= 
    let iv = DL.map (\ s -> (s,[])) subjects
    in DL.map (\ini -> (foldScore (\(sub,arr) (sname,k,scorelist)-> (sub,if k
                then if DL.length (DL.filter (\(subj,_,flag,_) -> (subj == sub) && (flag==True)) scorelist) > 0
                    then arr ++ [sname]
                    else arr
                else arr)
                ) ini marksheet subjects)) iv

subjectsInExam :: MarkSheet -> [String] -> [([SubjectName],[StudentName])]
subjectsInExam  marksheet subjects=
    let namelist = foldScore (\arr (sname,k,scorelist)-> if k
                    then arr ++ [(sname,DL.foldl'(\ ar (sub,_,flag,_) -> if flag
                        then ar ++ [sub]
                        else ar
                    ) [] scorelist)]
                    else arr
                ) [] marksheet subjects
        sublist = DL.map (\ini -> (foldScore (\(sub,arr) (sname,k,scorelist)-> (sub,if k
                    then if DL.length (DL.filter (\(subj,_,flag,_) -> (subj == sub) && (flag==True)) scorelist) > 0
                        then arr ++ [sname]
                        else arr
                    else arr)
                    ) ini marksheet subjects)) (DL.map (\ s -> (s,[])) subjects)
        newname = delete [] (nub (DL.map (\(_,y) -> y) namelist))
        newsub =  delete [] (nub (DL.map (\(_,y) -> y) sublist))
        emptyele = DL.filter (\(_,y)-> y == []) sublist
        emptylist = DL.map (\(x,y) -> ([x],y)) emptyele
        newlist = DL.zip newname newsub
    in newlist ++ emptylist
    
    -- function to recurse through case
calcOptions :: ByteString -> ByteString -> IO()
calcOptions mksheetOp subOp =
    do
        Prelude.putStrLn "Enter 1 for avg Marks\nEnter 2 for Standard deviation\nEnter 3 to get Duplicate names\nEnter 4 to get Invalid scores in subjects\nEnter 5 for Students taking exam in a subject\nEnter 6 for Students that have taken same exams"
        ch<-Prelude.getLine
        let mkStrn = formatMksheet mksheetOp
            subjects = formatSub (DB.unpack subOp)
        case mkStrn of
                Right mkStr->
                    do
                        case ((readMaybe ch)::Maybe Int) of
                            Just 1-> getSubAvg mkStr subjects
                            Just 2-> getStdDev mkStr subjects
                            Just 3-> getDuplicates mkStr subjects
                            Just 4-> getInvalidScores mkStr subjects
                            Just 5-> getStudentsInSubject mkStr subjects
                            Just 6-> getSubjectsInExam mkStr subjects
                            _-> Prelude.putStrLn "Invalid Choice"
                        calcOptions mksheetOp subOp
                Left err -> do
                    Prelude.putStrLn err
                    displayMkList
-- Monads part starts here

-- main display function
displayMkList :: IO()
displayMkList = do
    Prelude.putStrLn "Enter filename:"
    mkFile<-Prelude.getLine
    Prelude.putStrLn "Enter Subject filename:"
    sbFile<-Prelude.getLine
    mkOp<-(DB.readFile mkFile)
    sublist<-(DB.readFile sbFile)
    calcOptions mkOp sublist

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
    opStr ++ nme ++ "\n") "" (duplicateNames mksheet subjects))

getInvalidScores :: MarkSheet -> [String] -> IO()
getInvalidScores mksheet subjects = Prelude.putStrLn (DL.foldl' (\opstr (sname,list)-> 
    DL.foldl' (\str (a,b,c,d)-> 
        str++sname++" | "++a++" | "++show b++" | "++show c++" | "++d++"\n") opstr list
    ) "" (invalidScores mksheet subjects))

getStudentsInSubject :: MarkSheet -> [String] -> IO()
getStudentsInSubject mksheet subjects = Prelude.putStrLn (DL.foldl' (\opstr (sname,list)-> 
    opstr ++"\n"++sname++" | " ++ DL.foldl' (\str (studname)-> 
        str++studname++" ") "" list
    ) "" (studentsInSubject mksheet subjects))

getSubjectsInExam :: MarkSheet -> [String] -> IO()
getSubjectsInExam mksheet subjects = Prelude.putStrLn (DL.foldl' (\opstr (subList,snameList)-> 
        let subL = DL.foldl' (\str (subname)-> 
                str++subname++" | ") "" subList
        in opstr ++"\n"++ subL ++ DL.foldl' (\str (stname)-> 
            str++stname++" ") "" snameList
    ) "" (subjectsInExam mksheet subjects))

--converts to marsheet format
formatMksheet :: ByteString -> Either String [(String,[(String,Integer)])]
formatMksheet str = 
    let 
        dec=DC.decodeByName (BL.fromStrict str)
    in case dec of
        Right (h,val) -> 
            let 
                gdec = V.map (\(EScore sname sbname mrks)->[sname,sbname,(show mrks)]) val
                ldec = DL.groupBy (\x y -> (DL.head x) == (DL.head y)) (V.toList gdec)
            in mapM (\recrd-> (studEntry recrd ("",[]))) ldec
        Left err -> Left err

studEntry :: [[String]]->(String,[(String,Integer)])->Either String (String,[(String,Integer)])
studEntry recrd (snm,slst)= 
    if recrd == []
        then Right (snm,slst)
    else 
        case (DL.head recrd) of
            (sname:sbname:mark:[]) ->
                case ((readMaybe mark) :: Maybe Integer) of
                    Just mk -> if snm == ""
                        then studEntry (DL.tail recrd) (sname,[(sbname,mk)])
                        else studEntry (DL.tail recrd) (snm,slst++[(sbname,mk)])
                    Nothing-> Left "Marks not proper"
            _-> Left "Format not proper"

--gets subject csvs in list of strings
formatSub :: String -> [String]
formatSub subOp = fSub [] subOp
    where fSub subs remstr = if remstr == ""
            then subs
            else
                let (sbname,nxt) = (DL.break (==',') remstr)
                    nxtrecord = if nxt== "" then "" else DL.tail nxt
                in fSub (subs ++ [sbname]) nxtrecord