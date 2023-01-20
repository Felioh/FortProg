import Data.Maybe


listomaximo = [j*i | i <- [1 .. 5], j <-[1 .. 5] ]


list1= [[i * 2 + 1]  | i <- [1 .. 5]]

list2 = [(i*5,even (i*5)) | i <- [1 .. 5], elem i [1,4,5]]

list3 = [Just (i*i) | i <- [1 .. 5], odd i ]

list4 = [(i,j) | i <- [1 .. 5], j <- [5,4,3,2] , j > i ]

{- 
[(1,5),(1,4),(1,3),(1,2),(2,5),(2,4),(2,3),(3,5),(3,4),(4,5)] -}

mapList:: (a -> b) -> [a] -> [b]
mapList f list = [f i | i <- list]


lookupList:: Eq a => a -> [(a, b)] -> Maybe b
lookupList n list = listToMaybe [snd i | i <- list, n == fst i]
  {-       where listToMaybe []  = Nothing
              listToMaybe (x:_) = Just x -}               


{- replicateList:: Int -> a -> [a]
replicateList n x = [i | i <- [x]0] -}

replicateLC n x = [x | i <- [1 .. n]]   -- FEEEEEEEEEEEEELLIX???

filterList:: (a -> Bool) -> [a] -> [a]
filterList f list = [i | i <- list, f i]

