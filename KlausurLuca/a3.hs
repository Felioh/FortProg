-- contsructor: Frage, Liste von Antwortmöglichkeiten, Index richtige Antwort.
-- Immer genau eine ist richtig.
data Question = Question String [String] Int 
    deriving Show

f1 = Question "Was ist 1+1 ?" ["2, 3"] 0
f2 = Question "Was ist 2*4 ?" ["2, 5, 8"] 2

questions :: [Question]
questions = [f1, f2]

runAll :: [IO a] -> IO [a]
runAll [] = pure []
runAll (a:as) = a >>= \y -> runAll as >>= \ys -> return (y:ys) -- gibt das Resultat weiter.



alp = ['a' .. 'z']

askQuestion :: Question -> IO String
askQuestion (Question q as i) = do putStrLn q 
                                -- map putStr (take (lenght as) alp)
                                map putStrLn as
                                
                                -- ... hier printen wir auf jeden Fall irgendwie die Fragen
                                
                         		guess <- getLine
                           		if not elem guess (take (lenght as) alp) then return "Please choose a character between 'a' and '"++ last (take (lenght as) alp) ++"'."
                                                    else if elem guess (take (lenght as) alp) then return guess
                                                    else askQuestion question       
-- to be continued ...

-- main = do
    
    
