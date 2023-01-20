hangman (c:cs) = gameLoop (c:cs) "" 0



gameLoop:: String -> String -> Int -> IO()
gameLoop word ""    counter  =  do putStrLn ("Secret: " ++ secret word "" ' ')
                                   putStr "Enter a Character: "
                                   guess <- getChar
                                   let actualNew = secret word "" guess 
                                        in do putStr ("\n" ++"Secret: " ++ actualNew)
                                              gameLoop word actualNew (counter+1)
gameLoop word actual counter =  if word == actual then putStrLn ("Secret solved!" ++ "\n" ++ show(counter))
                                                  else do  putStr "Enter a Character: "
                                                           guess <- getChar
                                                           let actualNew = secret word actual guess 
                                                                    in do putStrLn ("\n" ++"Secret: " ++ actualNew)
                                                                          gameLoop word actualNew (counter+1)



secret:: String -> String -> Char -> String
secret []     _      _     =  ""
secret (s:ss) actual guess = if elem s actual || s == guess then s: secret ss actual guess 
                                                            else '*': secret ss actual guess


{- ghci> hangman "hallo"
Secret: *****
Enter a character: H
Secret: h****
Enter a character: l
Secret: h*ll*
Enter a character: o
Secret: h*llo
Enter a character: e
Secret: h*llo
Enter a character: a
Solved in 5 tries. -}