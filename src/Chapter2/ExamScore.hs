module Chapter2.ExamScore where
import Data.Char
import Data.List

type StudentName = String
type SubjectName = String
type SubjectMarks = Int
type MarkSheet = [(String,[(String,Int)])]
subAvg :: MarkSheet -> [SubjectName] -> SubjectName -> Int
subAvg mksheet subjects subname = let subarr = (filter (\x -> (fst x == subname)) (foldl' (\arr x -> arr ++ (snd x)) [] (allValidNames mksheet subjects)))
                in (foldl' (\s x -> (s+(snd x))) 0 subarr) `div` length subarr

checkNames :: MarkSheet -> StudentName -> Bool
checkNames mksheet studname = length (filter (\x -> (x == studname)) (foldl' (\arr x -> arr ++ [fst x]) [] mksheet)) == 1

checkScore :: MarkSheet -> [SubjectName] -> StudentName -> SubjectName -> (Bool,String)
checkScore mksheet subjects studname subname = let mark = snd (head (filter (\x -> (fst x == subname)) (snd (head (filter (\x -> (fst x) == studname) mksheet)))))
                                                    in if (mark < 0)
                                                        then (False,"negative score")
                                                        else if (mark > 100)
                                                        then (False,"greater than 100")
                                                        else if not (subname `elem` subjects)
                                                        then (False,"invalid subject name")
                                                        else (True,"Valid")
                                                        
calculateSd :: MarkSheet -> [SubjectName] -> SubjectName -> Int
calculateSd mksheet subjects subname = let avg = subAvg mksheet subjects subname
                                           subarr = (filter (\x -> (fst x == subname)) (foldl' (\arr x -> arr ++ (snd x)) [] (allValidSubjects mksheet subjects)))
                                            in (sum (foldl' (\arr x -> arr ++ [avg - (snd x)]) [] subarr)) `div` (length subarr)

duplicateNames :: MarkSheet -> [StudentName]
duplicateNames mksheet = let arr = (foldl' (\arr x -> arr ++ [fst x]) [] mksheet)
                            in (filter (\x -> (not (checkNames mksheet x))) arr)

invalidSubjects :: MarkSheet -> [SubjectName] -> StudentName -> [(StudentName,SubjectMarks,String)]
invalidSubjects mksheet subjects studname = foldl' (\arr x -> if (fst (checkScore mksheet subjects studname (fst x)))== False then arr ++ [(fst x,snd x,snd (checkScore mksheet [] studname (fst x)))] else arr) [] (snd (head (filter (\x -> (fst x)== studname) mksheet)))
validSubjects :: MarkSheet -> [SubjectName] -> StudentName -> [(SubjectName,SubjectMarks)]
validSubjects mksheet subjects studname = foldl' (\arr x -> if (fst (checkScore mksheet subjects studname (fst x)))== True then arr ++ [(fst x,snd x)] else arr) [] (snd (head (filter (\x -> (fst x)== studname) mksheet)))
allValidSubjects :: MarkSheet -> [SubjectName] -> MarkSheet
allValidSubjects mksheet subjects = foldl' (\arr x -> arr ++ [((fst x),(validSubjects mksheet subjects (fst x)) )]) [] mksheet
allValidNames :: MarkSheet -> [SubjectName] -> MarkSheet
allValidNames mksheet subjects = let newmksheet = allValidSubjects mksheet subjects
                            in filter (\x -> not ((fst x) `elem` (duplicateNames newmksheet))) newmksheet

invalidEntries :: MarkSheet -> [SubjectName] -> [(StudentName,[(SubjectName,SubjectMarks,String)])]
invalidEntries mksheet subjects = foldl' (\arr x -> 
    if (not (checkNames mksheet (fst x)))
        then arr ++ [(fst x,[("",0,"name duplicated")])]
    else if ((invalidSubjects mksheet subjects (fst x)) /= [])
        then arr ++ [(fst x,(invalidSubjects mksheet subjects (fst x)))]
    else arr) [] mksheet

subjectInExam :: MarkSheet -> [SubjectName] -> StudentName -> [(SubjectName,SubjectMarks)]
subjectInExam mksheet subjects studname = foldl' (\arr x -> arr ++ snd x) [] (filter (\x -> (fst x)==studname) (allValidNames mksheet subjects))
studentInExam :: MarkSheet -> [SubjectName] -> StudentName -> SubjectName -> Bool
studentInExam mksheet subjects studname subname = (filter (\x -> (fst x)==subname) (subjectInExam mksheet subjects studname)) /= []
allStudentsInExam :: MarkSheet -> [SubjectName] -> SubjectName -> (SubjectName,[StudentName])
allStudentsInExam mksheet subjects subname = foldl' (\(subname,sub) x ->
    if (studentInExam mksheet subjects (fst x) subname)
        then (subname, sub ++ [fst x])
    else  (subname, sub)) (subname,[]) (allValidNames mksheet subjects)
allSubjectsInExam :: MarkSheet -> [SubjectName] -> [(SubjectName,[StudentName])]
allSubjectsInExam mksheet subjects = foldl' (\arr x -> arr ++ [(allStudentsInExam mksheet subjects x)]) [] subjects


main :: IO()
main = print (invalidEntries [("Saurabh Nanda", [("English", 84), ("Chemisty", 80), ("Physics", 95), ("Geography", 75)]), ("John Doe", [("Chemisty", 80), ("Physics", 95), ("Geography", 75)]), ("Jane Doe", [("Chemisty", 66), ("Phsyics", 33), ("Geography", 56)]), ("John Doe", [("Chemisty", 90), ("Economics", 45), ("Geography", 56)]), ("Bahubali", [("Hindi", 45), ("Biology", -90), ("Geography", -75)]), ("Rajnikant", [("Tamil", 110), ("Biology", 100), ("Geography", 100)])] ["English","Geography","Physics","Chemistry","Economics","Computer Science"])
