data Grade = A | B | C | D | E
  deriving (Eq, Show)

type Name = String
type Points = Int

data Submission = Sub Name Points
  deriving Show
data Module = FPP | FP8 | FPK | EFP
data AttendanceList = AList [(Name, Module)]

subs :: [Submission]
subs = [Sub "Maximilian Wiendl" 14, Sub "Daniel Müller" 25, Sub "Sandra Maier" 25]

alist :: AttendanceList
alist = AList [("Maximilian Wiendl", EFP), ("Daniel Müller", FP8), ("Sandra Maier", FPP)]

maxPoints :: Module -> Points
maxPoints m = case m of
                FPP -> 50
                FP8 -> 50
                FPK -> 40
                EFP -> 28

grade :: Points -> Points -> Grade
grade points max = grade' (toEnum points / toEnum max)
  where grade' p | p >= 0.875             = A
                 | p < 0.875 && p >= 0.75 = B
                 | p < 0.75 && p >= 0.625 = C
                 | p < 0.625 && p >= 0.5  = D
                 | p < 0.5                = E


gradeSubmissions :: [Submission] -> AttendanceList -> [(Submission, Grade)]    
gradeSubmissions []                         _             = []     
gradeSubmissions ((Sub namesub points):xs) (AList (y:ys)) = if namesub == fst y then (Sub namesub points, grade points (maxPoints (snd y))) : gradeSubmissions xs (AList (y : ys))
                                                                                else findName (Sub namesub points) (AList ys) : gradeSubmissions xs (AList (y:ys)) 
    where findName (Sub namesub points) (AList (n:ns)) = if namesub == fst n then (Sub namesub points, grade points (maxPoints (snd n)))
                                                                              else findName (Sub namesub points) (AList ns)  

gradeDistribution :: [Grade] -> [(Grade, Int)]
gradeDistribution []     = []
gradeDistribution (x:xs) = (A, (count A 0 (x:xs))) : ([(B,(count B 0 (x:xs)))] ++ [(C,(count C 0 (x:xs)))] ++ [(D,(count D 0 (x:xs)))] ++ [(E,(count E 0 (x:xs)))]) 
    where count grade acc (x:xs) = if x == grade then count grade (acc+1) xs 
                                                 else count grade acc xs    
          count g acc []         = acc
          --TODO Wenn Note nicht enthalten, wird sie trotzdem noch aufgeführt