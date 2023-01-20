import Data.Char

hangman :: String -> IO()
hangman word = game word []

game :: String -> String -> IO()
game word inp = if (pr word inp) == word then putStrLn ("Solved in "++ show (length inp) ++ "tries")
                else do putStrLn (pr word inp) 
                        putStr "Enter a char: " 
                        guess <- getChar 
                        game word (guess : inp)

pr :: String -> String -> String
pr [] g = []
pr (c : w) g = if not (elem (toLower c) g) && not (elem (toUpper c) g) then '*' : pr w g
                                                                             else c : pr w g
