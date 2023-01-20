data Rose a = Rose a [Rose a]

--instance Eq:: Rose a -> Rose a -> Bool
instance Eq a => Eq (Rose a) where
    Rose x ys  == Rose b cs = x == b && ys == cs
{-  Rose x []  == Rose b [] = x == b
    Rose x []  == Rose b cs = False
    Rose x ys  == Rose b [] = False -}
 
instance Ord a => Ord (Rose a) where 
    x  <= y = x < y || x == y
    Rose x ys  <  Rose b cs = (x < b || ys < cs)  && (x <= b && ys <= cs)
    Rose x ys  >  Rose b cs = (x > b || ys > cs)  && (x >= b && ys >= cs)
    x  >= y = x > y || x == y



    
baumo0 :: Rose Int
baumo0 = Rose 5 [Rose 5 [Rose 1 [], Rose 2 [Rose 7 [], Rose 8 []], Rose 3 []], Rose 8 []]
baumo1 :: Rose Int
baumo1 = Rose 4 [Rose 5 [Rose 1 [], Rose 2 [Rose 7 [], Rose 8 []], Rose 3 []], Rose 7 []]



class Pretty a where
    pretty :: a -> String
  

instance Show a => Pretty (Rose a) where 
    pretty (Rose x xs) = pretty2 (Rose x xs) 0
        where pretty2  (Rose y as) deep = prettyList as deep ((show y) ++ "\n")
              prettyList (x:xs)        deep result = prettyList xs deep (result ++  (concat (replicate deep "|   "))++ "+--" ++ (pretty2 x (deep+1)))
              prettyList []            deep result = result
               
                  


{- 
ghci> putStrLn (pretty (Rose 4 [Rose 5 [Rose 1 [], Rose 2 [Rose 7 [], Rose 8 []], Rose 3 []], Rose 6 []]))
4
+-- 5
|   +-- 1
|   +-- 2
|   |   +-- 7
|   |   +-- 8
|   +-- 3
+-- 6
 -}


