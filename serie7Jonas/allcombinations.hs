{- allCombinations :: [a] -> [[a]]
allCombinations []     = [[]]
allCombinations list = [] : combine (calc list) 
                      --calc:: [a] -> [[a]]
                where   calc []     = []
                        calc (x:xs) = [x] : calc xs
                                  --combine:: [[a]]-> [[a]]
                            where combine (l:ls) =   -}

allCombinations :: [a] -> [[a]]
allCombinations []     = [[]]
allCombinations values = [] : concatMap (\w -> map (:w) values) (allCombinations values)
                            