mapp::(a -> b) -> [a] -> [b]
--mapp f list = foldl (\ls y -> (f y):ls ) [] list
mapp f list = foldr ((:).f) [] list

 {- [Int] -> Int -> Int  -> [Int]
(:). +1 -}

reversee:: [a] -> [a]
reversee list = foldl (flip(:)) [] list

unzipp:: [(a, b)] -> ([a],[b])
unzipp list = foldr (\(x,y) (xs,ys) -> (x:xs,y:ys)) ([],[]) list

nubb::Eq a => [a] -> [a]
nubÖb list = foldr (\x ys -> x: (filter (x/=) ys) ) [] list



listoGrando = [(1,2),(3,4),(5,6)]

listo:: [Integer]
listo = [1,2,3]
--[3,2,1]
--(1:(2:(3:4)))

--(1:(2:(3:(4:(5:(6:[])))))) foldr 

--(((((([]:1):2):3):4):5):6) foldl


