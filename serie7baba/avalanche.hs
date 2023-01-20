{- {- avalanche :: [Integer]
avalanche = filter (\y -> ((\i -> 3^i)* (\j -> 5^j)* (\k -> 7^k)) == y)  (allnumbers 0) -}
avalanche :: [Integer]
avalanche = calc dreier funfer siebener
                where calc (x:xi:xs) (y:yi:ys) (z:zi:zs) | (xi*y*z < (x+yi*z)) && (xi*y*z < (x*y*zi)) = x*y*z : calc (xi:xs) funfer  siebener
                                                         | (x*yi*z < (xi+y*z)) && (x*yi*z < (x*y*zi)) = x*y*z : calc dreier  (yi:ys) siebener
                                                         | otherwise                                  = x*y*z : calc dreier  funfer  (zi:zs)
                    
               {-      if (x+1) < (y+1) && (z+1) then x*y*z : calc xs (y:ys) (z:zs)
                                                                             else if (y+1) < (x+1) && (z+1) then x*y*z : calc (x:xs) ys (z:zs)
                                                                                 else x*y*z : calc (x:xs) (y:ys) zs -}
dreier:: [Integer]
dreier = calc 0
     where calc  n = (3^n): calc(n+1)
 
funfer:: [Integer]
funfer = calc 0
     where calc  n = (5^n): calc(n+1)
           
siebener:: [Integer]
siebener = calc 0
     where calc  n = (7^n): calc(n+1)
       

allnumbers:: Integer -> [Integer]
allnumbers  0 = 0: allnumbers(0+1)
allnumbers  n = n: allnumbers(n+1)


schnittMenge:: Eq a => [a] -> [a]-> [a]
schnittMenge []     list = []
schnittMenge (x:xs) list = if elem x list   then x: (schnittMenge xs list)
                                            else schnittMenge xs list

{- filter (\y i j k -> (3^i * 5^j * 7^k) = y)
\y -> ((\i -> 3^i)* (\j -> 3^j)* (\k -> 3^k)) = y -}


{- ghci> take 10 avalanche 
[1,3,5,7,9,15,21,25,27,35] -}
menge1 = [1,2,3,4,5,6]
menge2 = [1,3,10,4,11] -}

         
avalanche :: [Integer]
avalanche = 1 : map (3 *) avalanche
        `merge` map (5 *) avalanche
        `merge` map (7 *) avalanche

merge :: [Integer] -> [Integer] -> [Integer]
merge []     ns     = ns
merge ms     []     = ms
merge (m:ms) (n:ns) = case compare m n of
  LT -> m : merge ms (n:ns)
  EQ -> m : merge ms ns
  GT -> n : merge (m:ms) ns