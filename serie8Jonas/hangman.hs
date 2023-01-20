
hangman word = gameloop word " " 0


gameloop:: String -> String -> Int -> IO()
gameloop word actualWord counter = if word == actualWord then putStrLn ("Solved in " ++ show counter ++ "tries")
                                                         else  do putStrLn "Enter a character: "
                                                                  x <- getChar
                                                                  let actualNew = secreter word actualWord x             -- warum muss hier let in und nicht einfach:              
                                                                      in do putStrLn ("Secret: " ++ actualNew)           -- actualNew <- secreter word actualWord x -- "in" nicht nötig
                                                                            gameloop word actualNew (counter+1)          -- putStrLn ("Secret: " ++ actualNew)




secreter:: String -> String -> Char -> String
secreter []       actualWord guess  = ""
secreter (s:ss)   actualWord guess    = if elem s actualWord || s == guess  then s:  secreter ss actualWord guess
                                                                            else '*': secreter ss actualWord guess 