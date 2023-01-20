import Data.Char

hangman :: String -> IO ()
hangman s = game s []


game :: String -> String -> IO ()
game word input = if (printOut word input) == word then putStrLn ("Solved in " ++ show (length input) ++ " tries")
                                                    else do putStrLn (printOut word input)
                                                            putStr "Enter a Character: "
                                                            guess <- getChar
                                                            putStrLn ""
                                                            game word (guess : input)

printOut :: String -> String -> String
printOut [] g = []
printOut (c : w) g = if not (elem (toLower c) g) && not (elem (toUpper c) g) then '*' : printOut w g
                                                                             else c : printOut w g
