{- --3,6,7
--AUFGABE 3
data Grade = A | B | C | D | E
  deriving (Eq, Show)

type Name = String
type Points = Int

data Submission = Sub Name Points
data Module = FPP | FP8 | FPK | EFP
data AttendanceList = AList [(Name, Module)]

subs :: [Submission]
subs = [Sub "Jonas Beckmann" 10, Sub "Herbert Coolman" 8, Sub "Maike Coolfrau" 7]

alist :: AttendanceList
alist = AList [("Jonas Beckmann", EFP), ("Herbert Coolman", FPK), ("Maike Coolfrau", FPP)]

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
gradeSubmissions [] x = []
gradeSubmissions (x : xs) (AList as) = (searchInList x as) : (gradeSubmissions xs (AList as))

--Hilfsfunktion für grade Submissions
searchInList :: Submission -> [(Name, Module)] -> (Submission, Grade)
searchInList (Sub name points) (x : xs) = if name == fst x then ((Sub name points), grade points (maxPoints (snd x)))
                                                           else searchInList (Sub name points) xs
searchInList sub []                     = (sub, E)

-- 3.
 
gradeDistribution :: [Grade] -> [(Grade, Int)]      --TODO nocht nicht ganz richtig
gradeDistribution gs = (calc A gs 0) ++ (calc B gs 0) ++ (calc C gs 0) ++ (calc D gs 0) ++ (calc E gs 0)
    where calc x (g : gs) c = if x == g then calc gs (c+1) else calc x g c
          calc x [] c       = if c /= 0 then [(x, c)] else []

-- 4.
 
publishGrades :: [Submission] -> AttendanceList -> IO ()                                                 
publishGrades xs alist = do dis <- gradeDistribution (snd (unzip (gradeSubmissions xs)))                
                            calc dis
    where calc (d : dis) = putStrLn d >>= calc dis
          calc []        = return


 -}

--AUFGABE 6
{- 
import Test.QuickCheck

data Number = Num [()]
  deriving (Show, Eq)

{- instance Functor Number where 
    fmap = mapNumber

mapNumber:: (Number -> Number) -> Number -> Number
mapNumber f [] = []
mapNumber f (x:xs) = f x : mapNumber f xs  -}
 
 --Eine Funktor Instance kann nicht erstellt werden, da es nur einen Konstruktor gibt und keine typvariable wie zum Beispiel 'a'. Eine mAp Funktion wäre somit nicht sinnvoll

add:: Number -> Number -> Number
add (Num xs) (Num ys) = (Num (xs ++ ys))

mult :: Number -> Number -> Number 
mult (Num (x:xs)) (Num listb) = Num ([x| a <-(x:xs), b <- listb]) 

 
subt :: Number -> Number -> Maybe Number --irgendein kleinen fehler finden
subt (Num []) _                = Nothing
subt (Num (x:xs)) (Num  [] )   = Just (Num (x:xs))             
subt (Num (x:xs)) (Num (y:ys)) = subt (Num (xs)) (Num (ys))

             
prop_test_add:: Number -> Bool
prop_test_add number = add (Num []) number == number

prop_test_mult:: Number -> Bool
prop_test_mult number = mult number (Num []) == Num []


--2^n
pow2 :: [Number]
pow2 = [mult ([(),()]) ([(),()])| i <- [0 .. j], j <-[0 .. ]]
  -}



--AUFGABE 7

data Number = Num [()]
    deriving (Show, Eq)

instance Ord Number where
    --compare (Number xs) (Number ys) = case compare xs ys of
    --EQ -> EQ
    --(<) = [] ys
    --(>) = ys []
    compare (Number xs) (Number ys) = compare (fromNumber (Number xs)) (fromNumber (Number ys))


modulo :: Number -> Number -> Number 
modulo number1 number2 = if number1 > number2 then modulo (subt number1 number2) number2
                                              else number1

                    

fromNumber :: Number -> Int        
fromNumber []       =
fromNumber (Num xs) = calc1 as 0

calc1 (x : xs) counter = calc1 xs (counter + 1)
calc1 [] counter       = counter


toNumber :: Int -> Maybe Number     
toNumber a | a >= 0    = Just (Num (calc2 a))
           | otherwise = Nothing
           
calc2 0 = []
calc2 a = () : calc2 (a-1)

