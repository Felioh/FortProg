reverse':: [a] -> [a]
reverse' list = foldr funktion [] list
        where funktion = \x xs -> xs ++ [x] 

{- 1:2:3:[]

1 (f 2 (f (3 (f [])))) -}

--unzip
unzip'::[(a,b)] -> ([a],[b])
unzip' list = foldr funktion ([],[]) list
        where funktion = \(x,y) (xs,ys) -> ((x:xs), (y:ys))

            

testlist = [(1,'a'), (2,'b')] -- -> ([1,2],[a,b])

--nub
nub:: [Int] -> [Int]
nub list = foldr funktion [] list
        where funktion = \x xs -> x: filter (/= x) xs

