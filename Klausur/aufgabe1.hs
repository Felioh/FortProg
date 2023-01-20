--Aufgabe 3
data Question = Question String [String] Int
                        --Frage .. Liste von Antwortmöglichkeiten, Index korrekter Antwort


questions :: [Question]
questions = [Question "Wie einfach fällt die Funktionale Programmierung?" ["Sehr gut", "Gut", "Schlecht"] 1, Question "Was gefällt dir besser als in Java?" ["Alles", "das meiste", "nichts"] 1]


runAll :: [IO a] -> IO [a]
runAll (x:xs) = do x <- Question
                   runAll xs 
                   return (x:xs) 
--Teilaufgabe 3
askQuestion :: Question -> IO String
askQuestion (Question s a i) = do putStrLn s
                                  putStrLn a

                                  userGuess <- getLine
                                  if userGuess == a then 
               

{- hilfsfun::Question -> IO Bool
hilfsfun  -}

--Teilaufgabe 4
main:: IO ()
main = do (x:xs) <- questions
          askQuestion (x:xs)

         
                       




--Aufgabe 6
--Teilaufgabe 1,2
{- foldX :: (a -> c) -> (Maybe c -> b -> Maybe c -> c) -> X a b -> c
foldX _ e  []    = e
foldX f1 f2 x  | x == a  = f1 x (foldr f1 f2 b)  
               | x == b  = f2 x (foldr f1 f2 a)  -}  

data X a b = Left a | Right b 

--Aufgabe 6 Teilaufgabe 3
data BExp = Const Bool | Var String | And BExp BExp | Or BExp BExp | Not BExp
    deriving Show 

normalize :: BExp -> BExp
normalize (Const x)       = (Const x)
normalize (Not (And x y)) = Or (Not x) (Not y)--Not (A AND B) => (Not A) OR (Not B)
normalize (Not (Or x y))  = And (Not x) (Not y)--Not (A Or B) => (Not A) AND (Not B)
normalize (Not(Not x))    = x

testExp = And (Or (Const True) (Const False)) (Const True)


--Aufgabe 6 Teilaufgabe 4
type Valuation = [(String, Bool)]

--Ergebnis Liste von Listen
{- allValuations :: [String] -> [Valuation]
allValuations []     = []
allValuations (x:xs) = ((x, True) : (allValuations xs)) ++ ((x,False) : (allValuations xs)) -}



{- allValuations :: [String] -> [Valuation]
allValuations []   = []
allValuations (x:xs) = [(x, True)] : [(x, False)] : allValuations xs -}





-- Aufgabe 7
--Teilaufgabe 1
vars :: BExp -> [String]
vars (And x y) = vars x ++ vars y
vars (Or x y)  = vars x ++ vars y
vars (Var x)   = [x]
vars (Not x)   = vars x
vars _         = []

--Teilaufgabe 2
value :: Valuation -> BExp -> Bool
value  v          (Const x)  = x
value ((x,y):xys) (Var s)    = stringValue ((x,y):xys) (Var s)
                            where stringValue ((x,y):xys) (Var s)   = if x == s then y
                                                                                else stringValue xys (Var s)
value v    (And a b)   = (value v a) && (value v b)
value v    (Or a b)    = (value v a) || (value v b)
value v    (Not a)     = not (value v a)
            
--Teilaufgabe 3
satisfiable :: BExp -> [Valuation]
satisfiable (Const x)   = [[(x, True)]]
satisfiable (And (x,y)) = [[(x, True),(y, True)]]
satisfiable (Or (x,y))  = [[(x, True),(y, True)],[(x, False),(y, True)][(x, True),(y, False)]]
satisfiable (Not x)     = [[(x, False)]]

