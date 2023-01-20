allCombinations :: [a] -> [[a]]
allCombinations ls = [] : calc ls ++ combi (calc ls) (calc ls)
     where combi (x:xs) elemntsList = foldi x  ++ combi (xs ++ foldi x) elemntsList
           calc []      = []
           calc (x:xs)  = [x] :  calc xs
           foldi x =  foldr (\a b -> (x ++ [a]):b) [] ls
                
               
                
                   {- calc element []     completeList = [] 
                   calc element (x:xs) completeList = [x]:(combine element completeList) ++ (calc x xs completeList)
                   combine x []     = []
                   combine x (y:ys) = [x,y] : combine x ys -}

{- [[True] , [False]]

[[True], [False], [True, True], [True, False]] -}

{- ghci> take 10 (allCombinations [True,False])
[[],[True],[False],[True,True],[False,True],[True,False],[False,False],[True,True,True],[False,True,True],[True,False,True]] -}
