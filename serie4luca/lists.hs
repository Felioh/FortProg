reverse1 :: [a] -> [a]
reverse1 [] = []
reverse1 (x:xs) = reverse1 xs ++ [x]

reverse2 :: [a] -> [a]
reverse2 xs = rev [] xs
    where rev cs []     = cs
          rev cs (x:xs) = rev (x:cs) xs

-- list, elem -> index
indexOf :: [Int] -> Int -> Maybe Int
indexOf [] elem = Nothing
indexOf ls elem = iter 0 ls elem
    where iter idx [] elem     = Nothing                      
          iter idx (l:ls) elem = if l == elem then Just idx
                                 else iter (idx+1) ls elem 

inits :: [a] -> [[a]]
inits ls = make [[]] ls
    where make c []     = c
          make c (l:ls) = make (c ++ [last c ++ [l]]) ls

tails :: [a] -> [[a]]
tails []     = [[]]
tails (x:xs) = (x:xs) : tails xs

insert :: a -> [a] -> [[a]]
insert elem list = insert' [] list [elem : list] --starting with he first possible result
    where insert' _      []       result = result --done. gone through every element
          insert' start (a : end) result = insert' (start ++ [a]) end result ++ [(start ++ [a]) ++ (elem : end)] --putting the new element betweenour splittet list