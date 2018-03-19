{-# LANGUAGE ScopedTypeVariables #-}
module Monads.ExamScore where
import Data.List
import Control.Lens
type StudentName = String
type SubjectName = String
type SubjectMarks = Integer
type MarkSheet = [(String,[(String,Integer)])]

--Part 1
foldScore :: (b -> (StudentName,Bool,[(SubjectName,SubjectMarks, Bool, String)]) -> b) -> b -> MarkSheet -> [String] -> b
foldScore func accumulator mksheet subjects= 
    let validscore = over mapped (\(studname,scorelist)->(studname,foldl' (\arr (subname,mark) -> 
                    if (mark < 0)
                    then arr ++ [(subname,mark,False,"negative score")]
                    else if (mark > 100)
                    then arr ++ [(subname,mark,False,"greater than 100")]
                    else if (subname `notElem` subjects)
                    then arr ++ [(subname,mark,False,"invalid subject name")]
                    else arr ++ [(subname,mark,True,"Valid")]) [] scorelist)) mksheet
        dup = group (sort (over mapped (\x -> x^._1) validscore))
        getDup = over mapped (\x-> head x) (filter (\x -> (length x) > 1) dup)
        setDup = over mapped (\(x,y) -> if (x `elem` getDup) then (x,False,y) else (x,True,y)) validscore
    in foldl' (\acc x -> func acc x) accumulator setDup
    
subAvg :: MarkSheet -> SubjectName -> [String] -> Float
subAvg marksheet subjname subjects= 
    let tLen = foldScore (\(sumc,num) (_,k,scorelist) -> if k
            then
                foldl' (\acc flgMks -> if (flgMks^._1 == subjname) && (flgMks^._3)
                    then (acc^._1+flgMks^._2,acc^._2+1)
                    else acc) (sumc,num) scorelist
            else (sumc,num)
            ) (0,0) marksheet subjects
    in fromIntegral (tLen^._1) / (tLen^._2)

calculateSd :: MarkSheet -> SubjectName -> [String] -> Float 
calculateSd mksheet subjname subjects=
    let avg :: Float = subAvg mksheet subjname subjects
        mksarr :: [Float] = foldScore (\arr scrList -> 
                foldl' (\ar flgMks -> if (flgMks^._1 == subjname) && flgMks^._3
                then ar ++ [((**) (fromIntegral (flgMks^._2) - avg) 2)]
                else ar) arr (scrList^._3)
            ) [] mksheet subjects
        variance :: Float = (sum mksarr) / fromIntegral (length mksarr)
    in sqrt variance

duplicateNames :: MarkSheet -> [String] -> [StudentName]
duplicateNames marksheet subjects= 
    let list = foldScore (\arr flgLst -> if ((flgLst^._2)==False) then arr ++ [flgLst^._1] else arr) [] marksheet subjects
    in nub list

invalidScores :: MarkSheet -> [String] -> [(StudentName,[(SubjectName,SubjectMarks,Bool,String)])]
invalidScores marksheet subjects=
    let subs = foldScore (\arr flgSubs -> arr ++ [(flgSubs^._1,(filter (\(_,_,flag,_)-> flag==False)) (flgSubs^._3))]) [] marksheet subjects
    in subs

--Part2
studentsInSubject :: MarkSheet -> [String] -> [(SubjectName,[StudentName])]
studentsInSubject marksheet subjects= 
    let iv = over mapped (\ s -> (s,[])) subjects
    in over mapped (\ini -> (foldScore (\subArr flgMkLst-> (subArr^._1,if flgMkLst^._2
                then if length (filter (\flgSub -> ((flgSub^._1) == (subArr^._1)) && ((flgSub^._3)==True)) (flgMkLst^._3)) > 0
                    then subArr^._2 ++ [flgMkLst^._1]
                    else subArr^._2
                else subArr^._2)
                ) ini marksheet subjects)) iv
    
subjectsInExam :: MarkSheet -> [String] -> [([SubjectName],[StudentName])]
subjectsInExam  marksheet subjects=
    let namelist = foldScore (\arr (sname,k,scorelist)-> if k
                    then arr ++ [(sname,foldl'(\ ar (flgSubs) -> if (flgSubs^._3)
                        then ar ++ [flgSubs^._1]
                        else ar
                    ) [] scorelist)]
                    else arr
                ) [] marksheet subjects
        sublist = over mapped (\ini -> (foldScore (\(sub,arr) (sname,k,scorelist)-> (sub,if k
                    then if length (filter (\flgSubs -> ((flgSubs^._1) == sub) && ((flgSubs^._3)==True)) scorelist) > 0
                        then arr ++ [sname]
                        else arr
                    else arr)
                    ) ini marksheet subjects)) (over mapped (\ s -> (s,[])) subjects)
        newname = delete [] (nub (over mapped (\y -> (y^._2)) namelist))
        newsub =  delete [] (nub (over mapped (\y -> (y^._2)) sublist))
        emptyele = filter (\y-> (y^._2) == []) sublist
        emptylist = over mapped (\xy -> ([xy^._1],xy^._2)) emptyele
        newlist = zip newname newsub
    in newlist ++ emptylist