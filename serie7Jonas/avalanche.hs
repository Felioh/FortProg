{- intList:: [Integer]
intList = calc 0
            where calc n = n : calc (n+1)

avalanche :: [Integer]
avalanche = calc intList
            where calc (x:xs) = if elem x combine then x : calc xs
                                                  else calc xs


combine = [ 3^i * 5^j * 7^k| i <- [0 ..], j <- [0 ..], k <- [0 ..]]


dreier = 3^0 : calc 1
        where calc n = 3^n : calc (n+1) 

funfer = 5^0 : calc 1
        where calc n = 5^n : calc (n+1) 

siebener = 7^0 : calc 1
        where calc n = 7^n : calc (n+1) 

sortcombine = calc combine
                where calc (x:xs) = filter (<=x) xs ++ [x] ++ filter (>x) xs -}