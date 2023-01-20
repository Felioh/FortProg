
reverse1:: [a] -> [a]
reverse1 []     = []
reverse1 (x:xs) = reverse1 xs ++ [x]

reverse2:: [a] -> [a]
reverse2 list = calc list 0
                where calc []     n = []
                      calc (x:xs) n = calc xs (n+1) ++ [x]

indexOf:: Int -> [Int] -> Maybe Int
indexOf elem list = calc elem list 0
                    where calc elem []     n             = Nothing
                          calc elem (x:xs) n | x == elem = Just n
                                             | otherwise = calc elem xs (n+1)

listo :: [Int]
listo = [1,2,3,4,5,6,7,8,9]

listo1 = [1,2]

inits:: [a] -> [[a]]
inits list = calc list 0
            where calc list n = if n < (length list) then calc2 (take n list) ++ calc list (n+1)
                                                     else calc2 (take n list)
                        where calc2 []   = [[]]
                              calc2 list = [list]


tails:: [a] -> [[a]]
tails list = reverse1 (calc (reverse1 list) 0)
            where calc list n = if n < (length list) then calc2 (take n list) ++ calc list (n+1)
                                                     else calc2 (take n list)
                        where calc2 []   = [[]]
                              calc2 list = [reverse1 list]

insert :: a -> [a] -> [[a]]
insert elem []     = [[elem]]
insert elem (y:ys) = (elem:y:ys) : add y (insert elem ys)

add:: a -> [[a]] -> [[a]]
add elem []    = []
add elem (xs:xss) = (elem:xs) : add elem xss
            


perms :: [a] -> [[a]]
perms []     = [[]]
perms (x:xs) = permInsert [] (perms xs)
  where
    permInsert acc []       = acc
    permInsert acc (ys:yss) = permInsert (acc ++ insert x ys) yss
