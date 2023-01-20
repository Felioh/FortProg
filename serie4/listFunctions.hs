import SimplePrelude

test = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

reverse1 :: [generic] -> [generic]
reverse1 [] = [] -- Fall leere liste
reverse1 (x:xs) = reverse1 xs ++ [x]

--0.15 secs, 278,384 bytes) mit der obigen test Liste.

-- reverse a list using accumulation
reverse2 :: [generic] -> [generic]
reverse2 = li [] -- Fall leere Liste
    where li accumulator []       = accumulator
          li accumulator (x:xs)   = li (x:accumulator) xs

 --(0.01 secs, 206,064 bytes) mit der obigen test Liste.


indexOf ::Int -> [Int] -> Maybe Int           
indexOf a [] = Nothing
indexOf a xs = indexof2 0 a xs
        where indexof2 number a [] = Nothing
              indexof2 number a (x:xs) | a == x  = Just number 
                                       | otherwise = indexof2 (number+1) a xs
       


inits :: [a] -> [[a]]
inits list = init list [[]]
  where init [] result = result --returns the final result
        init (a : restlist) result = init restlist (result ++ [last result ++ [a]]) --adds the next result to our list of results
     

     
tails :: [a] -> [[a]]
tails list = tail' (reverse list) [[]]
  where tail' [] result = result --returns the final result
        tail' (a : restlist) result = tail' restlist [a : head result] ++ result -- adds the next result to our list of results



insert :: a -> [a] -> [[a]]
insert elem list = insert' [] list [elem : list] --starting with he first possible result
    where insert' _      []       result = result --done. gone through every element
          insert' start (a : end) result = insert' (start ++ [a]) end result ++ [(start ++ [a]) ++ (elem : end)] --putting the new element betweenour splittet list


{- perms :: [a] -> [[a]]
perms []         = [[]]
perms list = permute [] list list [[]]
  where permute _ [] full_list result = result
        permute start (b : end) full_list result = permute ([b] ++ start) end full_list (result ++ insert b (start ++ end)) -}
                                                 