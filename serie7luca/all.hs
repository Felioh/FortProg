allCombinations :: [a] -> [[a]]
allCombinations []    = [[]]
allCombinations elems = [] : concatMap (\y -> map (:y) elems) (allCombinations elems) 