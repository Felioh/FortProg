

reverse1:: [a] -> [a] 
reverse1 []     = []
reverse1 (x:xs) = reverse1 xs ++ [x]

reverse2::[a] -> [a] 
reverse2 list = calc list []
        where calc (x:xs) []     = calc xs (x:[])
              calc (x:xs) (y:ys) = calc xs (x:(y:ys))
              calc []     (y:ys) = (y:ys)

indexOf:: Int -> [Int] -> Maybe Int
indexOf element (x:xs) = calc element (x:xs) 0
        where calc element [] actualIndex                    = Nothing  
              calc element (x:xs) actualIndex | element == x = Just actualIndex 
                                              | otherwise    = calc element xs (actualIndex + 1)

inits:: [a] -> [[a]]
inits []     = [[]]
inits list = calc list []
        where calc []     solution = solution ++ [[]]
              calc (x:xs) []       = calc xs [[x]]
              calc (x:xs) solution = calc xs (solution ++ [last solution ++ [x]]) 

tails:: [a] -> [[a]]
tails []     = [[]]
tails list = calc (reverse1 list) []
        where calc []     solution = solution ++ [[]]
              calc (x:xs) []       = calc xs [[x]]
              calc (x:xs) solution = calc xs (solution ++ [last solution ++ [x] ]) 

insert :: a -> [a] -> [[a]]
insert t (x:xs) = calco t (x:xs) [[]] []
             where   calco t []      ys _       = ys
                     calco t (x:xs)  [] begin   = calco t xs [t:(x:xs)]  (begin ++ [x])     --anfang feht bei rekursivem aufruf
                     calco t (x:xs)  ys begin   = calco t xs ([begin ++ t:(x:xs)] ++ ys) ( begin ++ [x])                 --anfang feht bei rekursivem aufruf
                                                               

                                    --calco t xs (ys ++ [(begin ++ [x])]) ( begin ++ [x])  
                                    --calco t xs (([begin] ++ [t:(x:xs)]) ++ ys ) ( begin ++ [x]) 
                                    --(t:x:xs) ++ (x:calco t xs)

{- perms :: [a] -> [[a]]
perms (x:xs) = calco (x:xs) [] [[]]
                where calco (x:xs) []     [[]]     = calco xs [x] [(x:xs)]
                      calco (x:xs) begin  result   = calco xs (begin ++ [x]) [(begin ++ (x:xs))]++result  -}


perms :: [a] -> [[a]]
perms []     = [[]]
perms (x:xs) = permInsert [] (perms xs)
  where
    permInsert acc []       = acc
    permInsert acc (ys:yss) = permInsert (acc ++ insert x ys) yss


listo :: [Int]
listo = [1,2,3]

