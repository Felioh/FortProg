data Question = Question String [String] Int

a = Question "Was ist die Hauptstadt von Deutschland?" ["Berlin", "Hamburg", "Hannover"] 1
b = Question "Was ist 3 mal 3?" ["6", "9", "12"] 2

questions :: [Question]
questions =  [a, b]


--Vorraussetzung, dass eine Frage immer genau 3 Antwortmöglichkeiten hat
askQuestion :: Question -> IO String
askQuestion question = do putStrLn "" ++ fst (zip [a] [b])
                          putStrLn "a: " ++ fst (snd (zip [a] [b]))
                          putStrLn "b: " ++ snd (snd (zip [a] [b]))
                          putStrLn "c: " ++ thrd (snd (zip [a] [b]))
                          guess <- getLine
                          if notElem guess ["abc"] then return "Please choose a character between 'a' and 'c'" 
                                                   else if elem guess ["abc"] then return guess
                                                   else askQuestion question
                         




                          
